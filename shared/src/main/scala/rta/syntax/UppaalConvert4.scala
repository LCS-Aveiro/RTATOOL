package rta.backend

import rta.syntax.Program2
import rta.syntax.Program2.{Edge, QName, RxGraph}
import rta.syntax.{Condition, Statement, UpdateExpr, AssignStmt, ArrayAssignStmt, IfThenStmt, ForeachStmt, ReturnStmt, PrintStmt, RuntimeValue}
import scala.scalajs.js
import java.util.concurrent.atomic.AtomicInteger
import scala.xml._
import scala.collection.mutable

object UppaalConverter4 {

  case class Point(x: Double, y: Double)

  private def sanitize(name: String): String = name.replaceAll("[^a-zA-Z0-9_]", "_")
  private def sanitizeQName(qname: QName): String = sanitize(qname.show)

  private def exprToString(expr: UpdateExpr): String = expr match {
    case UpdateExpr.LitInt(i) => i.toString
    case UpdateExpr.LitFloat(f) => f.toString
    case UpdateExpr.LitBool(b) => if (b) "true" else "false"
    case UpdateExpr.LitArray(elems) => "{" + elems.map(exprToString).mkString(", ") + "}"
    case UpdateExpr.Var(q) => sanitizeQName(q)
    case UpdateExpr.ArrayAccess(arr, idx) => s"${sanitizeQName(arr)}[${exprToString(idx)}]"
    case UpdateExpr.MathOp(l, op, r) => s"${exprToString(l)} $op ${exprToString(r)}"
    case UpdateExpr.FuncCall(f, args) => s"${sanitizeQName(f)}(${args.map(exprToString).mkString(", ")})"
  }

  private def conditionToString(cond: Condition): String = cond match {
    case Condition.AtomicCond(l, op, r) => s"${exprToString(l)} $op ${exprToString(r)}"
    case Condition.And(l, r) => s"(${conditionToString(l)}) && (${conditionToString(r)})"
    case Condition.Or(l, r) => s"(${conditionToString(l)}) || (${conditionToString(r)})"
  }

  private def statementToString(stmt: Statement): String = stmt match {
    case AssignStmt(variable, expr) => s"${sanitizeQName(variable)} = ${exprToString(expr)};"
    case ArrayAssignStmt(arrName, index, expr) => s"${sanitizeQName(arrName)}[${exprToString(index)}] = ${exprToString(expr)};"
    case IfThenStmt(condition, thenStmts) =>
      val thenBlock = thenStmts.map(statementToString).map("\t" + _).mkString("\n")
      s"if (${conditionToString(condition)}) {\n$thenBlock\n}"
    case ForeachStmt(iter, arr, body) =>
      val bodyBlock = body.map(statementToString).map("\t" + _).mkString("\n")
      s"for (${sanitizeQName(iter)} : ${sanitizeQName(arr)}) {\n$bodyBlock\n}"
    case ReturnStmt(expr) => s"return ${exprToString(expr)};"
    case PrintStmt(_) => "// print not supported in UPPAAL"
  }

  private def stringToQName(str: String): QName = {
    if (str.isEmpty) Program2.QName(Nil)
    else Program2.QName(str.split('/').toList)
  }

  def convert(rxGraph: RxGraph, currentCode: String, layoutJson: String): String = {
    val layoutData = js.JSON.parse(layoutJson)
    val nodesPos = if (!js.isUndefined(layoutData.selectDynamic("nodes")) && layoutData.selectDynamic("nodes") != null) 
                 layoutData.selectDynamic("nodes") else js.Dictionary.empty[js.Dynamic].asInstanceOf[js.Dynamic]
    val edgesPos = if (!js.isUndefined(layoutData.selectDynamic("edges")) && layoutData.selectDynamic("edges") != null) 
                    layoutData.selectDynamic("edges") else js.Dictionary.empty[js.Dynamic].asInstanceOf[js.Dynamic]

    def getPos(id: String): Point = {
      val p = nodesPos.selectDynamic(id)
      if (!js.isUndefined(p) && p != null) Point(p.selectDynamic("x").asInstanceOf[Double], p.selectDynamic("y").asInstanceOf[Double])
      else Point(0, 0)
    }

    def calculateNails(sourceId: String, targetId: String, edgeId: String): List[Point] = {
      val e = edgesPos.selectDynamic(edgeId)
      if (js.isUndefined(e) || e == null) return Nil
      val dists = e.selectDynamic("distances").asInstanceOf[js.Array[Double]].toList
      val weights = e.selectDynamic("weights").asInstanceOf[js.Array[Double]].toList
      val s = getPos(sourceId); val t = getPos(targetId)
      val dx = t.x - s.x; val dy = t.y - s.y
      val length = Math.sqrt(dx*dx + dy*dy)
      if (length == 0) return Nil
      val nx = -dy / length; val ny = dx / length
      dists.zip(weights).map { case (d, w) => Point(s.x + w * dx + d * nx, s.y + w * dy + d * ny) }
    }

    val allStates = rxGraph.states.toList.sortBy(_.toString)
    val stateToId = allStates.zipWithIndex.map { case (qname, i) => qname -> s"id$i" }.toMap

    val actionLabels = rxGraph.edg.values.flatten.map(_._3).toSet.toList.sorted(Ordering.by[QName, String](_.toString))
    val labelToId: Map[QName, Int] = actionLabels.zipWithIndex.toMap

    val simpleEdges: List[Edge] = rxGraph.edg.flatMap { case (from, tos) =>
      tos.map { case (to, transId, lbl) => (from, to, transId, lbl) } 
    }.toList.distinct.sortBy(edge => (labelToId.getOrElse(edge._4, -1), edge._1.toString, edge._2.toString))
    val edgeToIndex: Map[Edge, Int] = simpleEdges.zipWithIndex.toMap

    type HyperEdgeIdentity = (String, QName, QName, QName, QName)
    
    val hyperEdges = (
      rxGraph.on.flatMap  { case (trigger, targets) => targets.map(t => ("on",  trigger, t._1, t._2, t._3)) } ++
      rxGraph.off.flatMap { case (trigger, targets) => targets.map(t => ("off", trigger, t._1, t._2, t._3)) }
    ).toList.distinct

    val hyperEdgeToIndex: Map[HyperEdgeIdentity, Int] = hyperEdges.zipWithIndex.toMap
    val memo = mutable.Map[QName, Set[QName]]()

    def findAllRootTriggers(trigger: QName): Set[QName] = {
      if (memo.contains(trigger)) return memo(trigger)
      if (labelToId.contains(trigger)) return Set(trigger)
      val result = hyperEdges
        .filter { case (_, _, _, _, ruleName) => ruleName == trigger }
        .flatMap { case (_, parentTrigger, _, _, _) => findAllRootTriggers(parentTrigger) }
        .toSet
      memo(trigger) = result; result
    }

    // DIVISÃO DE REGRAS: Imediatas vs Atrasadas (Delays)
    val immediateHyperEdges = hyperEdges.filterNot(h => rxGraph.delays.contains(h._5))
    val delayedHyperEdges = hyperEdges.filter(h => rxGraph.delays.contains(h._5))

    val arrayLInitializerEntries = immediateHyperEdges.flatMap { hEdge =>
      val (opType, triggerLbl, targetLbl, selfId, selfLbl) = hEdge
      val rootTriggers = findAllRootTriggers(triggerLbl)
      val effectType = if (opType == "on") "1" else "0"

      val simpleEdgeTargets = simpleEdges.filter(_._4 == targetLbl).map(e => (1, edgeToIndex(e))) 
      val hyperEdgeTargets = if (simpleEdgeTargets.nonEmpty) Nil else {
        immediateHyperEdges.filter { case (_, _, _, _, ruleName) => ruleName == targetLbl }
                           .flatMap { h_identity => hyperEdgeToIndex.get(h_identity).map(index => (0, index)) }
      }

      val status = if (rxGraph.act.contains((triggerLbl, targetLbl, selfId, selfLbl))) "1" else "0"
      
      for {
        root <- rootTriggers
        rootId = labelToId.getOrElse(root, -1)
        (isEdgeTarget, targetIndex) <- (simpleEdgeTargets ++ hyperEdgeTargets)
        if rootId != -1
      } yield s"    { $rootId, $effectType, $status, $isEdgeTarget, $targetIndex } /* Rule '${selfLbl.show}' */"
    }
    
    val finalNumHyperedges = if (arrayLInitializerEntries.distinct.isEmpty) 1 else arrayLInitializerEntries.distinct.size

    val functionCounter = new AtomicInteger(0)
    val dataFunctions = new StringBuilder

    val clockDecl = if (rxGraph.clocks.nonEmpty) s"clock ${rxGraph.clocks.map(sanitizeQName).mkString(", ")};" else ""
    val varDecl = rxGraph.val_env.map { case (q, v) => 
      val typeStr = v match { case _: RuntimeValue.VBool => "bool"; case _: RuntimeValue.VFloat => "double"; case _ => "int" }
      val valStr = v match { case RuntimeValue.VArray(elems, _, _) => "{" + elems.map(_.value).mkString(", ") + "}"; case _ => v.value.toString }
      val arrBrackets = v match { case RuntimeValue.VArray(_, _, maxOpt) => s"[${maxOpt.getOrElse(100)}]"; case _ => "" }
      s"$typeStr ${sanitizeQName(q)}$arrBrackets = $valStr;" 
    }.mkString("\n")

    val finalNumEdges = if (simpleEdges.isEmpty) 1 else simpleEdges.size
    val finalNumIds = if (actionLabels.isEmpty) 1 else actionLabels.size

    val declarationBuilder = new StringBuilder(
      s"""// -----------------------------------------------------------
        |// 1. Variáveis, Clocks e Canais Globais
        |// -----------------------------------------------------------
        |$clockDecl
        |$varDecl
        |
        |// Constantes do Sistema
        |const int NUM_EDGES = $finalNumEdges;
        |const int NUM_HYPEREDGES = $finalNumHyperedges;
        |const int NUM_IDS = $finalNumIds;
        |
        |// Canal de broadcast usado pelos Timers de Delay
        |broadcast chan action_event[NUM_IDS];
        |broadcast chan timeout_fired;
        |chan priority default < timeout_fired;
        |""".stripMargin)

    declarationBuilder.append(
      """
        |// -----------------------------------------------------------
        |// 2. Definições de Estrutura Reativa
        |// -----------------------------------------------------------
        |typedef struct { int id; bool stat; } Edge;
        |typedef struct { int id; bool type; bool stat; bool is_edge_target; int trg_index; } Hyperedge;
        |""".stripMargin)

    val arrayAInitializer = if (simpleEdges.isEmpty) "" else simpleEdges.map { edge =>
      val id = labelToId.getOrElse(edge._4, -1)
      val status = if (rxGraph.act.contains(edge)) "1" else "0"
      s"    { $id, $status } /* Idx ${edgeToIndex(edge)}: ${edge._1.show}->${edge._2.show}:${edge._4.show} */"
    }.mkString(",\n")

    declarationBuilder.append(
      s"""
         |// -----------------------------------------------------------
         |// 3. Inicialização dos Arrays
         |// -----------------------------------------------------------
         |Edge A[NUM_EDGES] = {\n$arrayAInitializer\n};
         |""".stripMargin)
    
    if (arrayLInitializerEntries.distinct.isEmpty) declarationBuilder.append(s"Hyperedge L[NUM_HYPEREDGES];\n")
    else declarationBuilder.append(s"Hyperedge L[NUM_HYPEREDGES] = {\n${arrayLInitializerEntries.distinct.mkString(",\n")}\n};\n")

    declarationBuilder.append(
      """
        |// -----------------------------------------------------------
        |// 4. Lógica de Atualização Reativa (Imediata e Atrasada)
        |// -----------------------------------------------------------
        |void update_hyperedges_by_id(int edge_id) {
        |    int i;
        |    for (i = 0; i < NUM_HYPEREDGES; i++) {
        |        if (L[i].id == edge_id && L[i].stat) { 
        |            if (L[i].is_edge_target) { A[L[i].trg_index].stat = L[i].type; } 
        |            else { L[L[i].trg_index].stat = L[i].type; }
        |        }
        |    }
        |}
        |
        |void update_delayed_rule(int is_edge, int idx, int effect) {
        |    if (is_edge) { A[idx].stat = effect; }
        |    else { L[idx].stat = effect; }
        |}
        |""".stripMargin)

    val locationNodes = allStates.map { stateName =>
      val stateId = stateToId(stateName)
      val pos = getPos(stateName.toString)
      val invariantNode = rxGraph.invariants.get(stateName)
        .map(cond => <label kind="invariant" x={pos.x.toString} y={(pos.y + 15).toString}>{conditionToString(cond)}</label>)
        .getOrElse(NodeSeq.Empty)

      <location id={stateId} x={pos.x.toString} y={pos.y.toString}>
        <name x={(pos.x - 20).toString} y={(pos.y - 30).toString}>{sanitizeQName(stateName)}</name>
        {invariantNode}
      </location>
    }

    val transitionNodes = simpleEdges.map { edge =>
        val (source, target,transId, lbl) = edge
        val edgeIndex = edgeToIndex(edge)
        val actionId = labelToId.getOrElse(lbl, -1)
        val actionNodeId = s"event_${source}_${target}_${transId}_${lbl}"

        val cyEdge1Id = s"s_to_a_${source}_${actionNodeId}"
        val cyEdge2Id = s"a_to_s_${actionNodeId}_${target}"
        val nails1 = calculateNails(source.toString, actionNodeId, cyEdge1Id)
        val actionNodePos = getPos(actionNodeId)
        val nails2 = calculateNails(actionNodeId, target.toString, cyEdge2Id)
        val allNails = nails1 ++ List(actionNodePos) ++ nails2
        val labelX = actionNodePos.x.toInt; val labelY = actionNodePos.y.toInt
        
        val reactiveGuard = s"A[$edgeIndex].stat == 1"
        val dataGuardOpt = rxGraph.edgeConditions.get(edge).flatten.map(conditionToString)
        val fullGuard = dataGuardOpt match { case Some(dg) => s"($reactiveGuard) && ($dg)"; case None => reactiveGuard }

        val statements = rxGraph.edgeUpdates.getOrElse(edge, Nil)
        val dataUpdateCall = if (statements.nonEmpty) {
            val funcName = s"update_data_${functionCounter.getAndIncrement()}"
            val funcBody = statements.map(statementToString).mkString("\n\t")
            dataFunctions.append(s"void $funcName() {\n\t$funcBody\n}\n")
            s"$funcName(), "
        } else ""
        
        val fullAssignment = s"${dataUpdateCall}update_hyperedges_by_id($actionId)"

        // ADICIONAR O SINAL DE BROADCAST SE EXISTIREM DELAYS OU POTENCIAL DE DELAYS
        val syncLabel = if (actionId >= 0) 
            <label kind="synchronisation" x={(labelX - 40).toString} y={(labelY - 55).toString}>action_event[{actionId}]!</label> 
        else NodeSeq.Empty

        <transition>
          <source ref={stateToId(source)}/>
          <target ref={stateToId(target)}/>
          <label kind="guard" x={(labelX - 40).toString} y={(labelY - 35).toString}>{fullGuard}</label>
          {syncLabel}
          <label kind="assignment" x={(labelX - 40).toString} y={(labelY + 15).toString}>{fullAssignment}</label>
          {allNails.map(p => <nail x={p.x.toInt.toString} y={p.y.toInt.toString}/>)}
        </transition>
    }

    declarationBuilder.append(
      s"""
         |// -----------------------------------------------------------
         |// 5. Funções de Dados (Geradas)
         |// -----------------------------------------------------------
         |${dataFunctions.toString()}
         |""".stripMargin)

    val initRef = rxGraph.inits.headOption.flatMap(stateToId.get)

    // CRIAR O TEMPLATE DO TIMER DE RECONFIGURAÇÕES
    val delayedRuleTemplate = if (delayedHyperEdges.isEmpty) NodeSeq.Empty else {
      <template>
        <name x="5" y="5">DelayedRule</name>
        <parameter>const int trigger_id, const int target_is_edge, const int target_idx, const int effect_type, const int delay_val</parameter>
        <declaration>clock t_del;</declaration>
        <location id="id_idle" x="-150" y="0">
           <name x="-160" y="-35">Idle</name>
        </location>
        <location id="id_wait" x="150" y="0">
           <name x="130" y="-35">Waiting</name>
           <label kind="invariant" x="130" y="15">t_del &lt;= delay_val</label>
        </location>
        <init ref="id_idle"/>
        <transition>
          <source ref="id_idle"/>
          <target ref="id_wait"/>
          <label kind="synchronisation" x="-60" y="-20">action_event[trigger_id]?</label>
          <label kind="assignment" x="-60" y="5">t_del = 0</label>
        </transition>
        <transition>
          <source ref="id_wait"/>
          <target ref="id_wait"/>
          <label kind="synchronisation" x="170" y="-20">action_event[trigger_id]?</label>
          <label kind="assignment" x="170" y="5">t_del = 0</label>
          <nail x="210" y="-40"/>
          <nail x="210" y="40"/>
        </transition>
        <transition>
       <source ref="id_wait"/>
       <target ref="id_idle"/>
       <label kind="guard" x="-60" y="40">t_del &gt;= delay_val</label>
       <label kind="synchronisation" x="-60" y="20">timeout_fired!</label>
       <label kind="assignment" x="-100" y="60">update_delayed_rule(target_is_edge, target_idx, effect_type)</label>
       <nail x="0" y="40"/>
     </transition>
      </template>
    }

    // INSTANCIAR OS TIMERS NA SECÇÃO DO SYSTEM
    val delayedInstantiations = delayedHyperEdges.zipWithIndex.map { case (hEdge, idx) =>
      val (opType, triggerLbl, targetLbl, _, ruleLabel) = hEdge
      val delayVal = rxGraph.delays(ruleLabel)._2.toInt // O UPPAAL só aceita Inteiros no tempo dos Timer Automata!
      val triggerId = labelToId.getOrElse(triggerLbl, -1)
      val effectType = if (opType == "on") 1 else 0

      val simpleEdgeTargets = simpleEdges.filter(_._4 == targetLbl).map(e => (1, edgeToIndex(e)))
      val allTargets = if (simpleEdgeTargets.nonEmpty) simpleEdgeTargets else {
          immediateHyperEdges.filter(_._5 == targetLbl).map(h => (0, hyperEdgeToIndex(h)))
      }
      
      // Cria uma declaração de instância por cada alvo que a regra atinja
      allTargets.map { case (isEdge, targetIdx) =>
         s"Delay_${sanitizeQName(ruleLabel)} = DelayedRule($triggerId, $isEdge, $targetIdx, $effectType, $delayVal);"
      }.mkString("\n")
    }.mkString("\n")

    val systemInstances = "Process" + (if (delayedHyperEdges.nonEmpty) ", " + delayedHyperEdges.map(h => s"Delay_${sanitizeQName(h._5)}").mkString(", ") else "")
    
    // CORREÇÃO AQUI: Adicionar Process = Template(); antes das instanciações!
    val systemStr = s"""Process = Template();\n$delayedInstantiations\nsystem $systemInstances;"""

    val nta =
      <nta>
        <declaration>{declarationBuilder.toString()}</declaration>
        <template>
          <name x="5" y="5">Template</name>
          {locationNodes}
          {initRef.map(ref => <init ref={ref}/>).getOrElse(NodeSeq.Empty)}
          {transitionNodes}
        </template>
        {delayedRuleTemplate}
        <system>{systemStr}</system>
      </nta>

    val pp = new PrettyPrinter(200, 2)
    val formattedXml = pp.format(nta).replace("&amp;&amp;", "&&").replace("&&", "&amp;&amp;")

    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
    "<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.6//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_6.dtd'>\n" +
    formattedXml.replace("  ", "\t")
  }
}