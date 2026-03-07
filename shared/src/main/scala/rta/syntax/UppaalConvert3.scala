package rta.backend

import rta.syntax.Program2
import rta.syntax.Program2.{Edge, QName, RxGraph}
import rta.syntax.{Condition, Statement, UpdateExpr, UpdateStmt, IfThenStmt}
import scala.scalajs.js
import scala.scalajs.js.JSON
import java.util.concurrent.atomic.AtomicInteger
import scala.xml._
import scala.collection.mutable

object UppaalConverter3 {

  case class Point(x: Double, y: Double)
  case class EdgeLayout(distances: List[Double], weights: List[Double])


  private def sanitize(name: String): String = name.replaceAll("[^a-zA-Z0-9_]", "_")
  private def sanitizeQName(qname: QName): String = sanitize(qname.show)

  private def exprToString(expr: UpdateExpr): String = expr match {
    case UpdateExpr.Lit(i) => i.toString
    case UpdateExpr.Var(q) => sanitizeQName(q)
    case UpdateExpr.Add(v, Right(q)) => s"${sanitizeQName(v)} + ${sanitizeQName(q)}"
    case UpdateExpr.Add(v, Left(i)) => s"${sanitizeQName(v)} + $i"
    case UpdateExpr.Sub(v, Right(q)) => s"${sanitizeQName(v)} - ${sanitizeQName(q)}"
    case UpdateExpr.Sub(v, Left(i)) => s"${sanitizeQName(v)} - $i"
  }

  private def conditionToString(cond: Condition): String = cond match {
    case Condition.AtomicCond(l, op, r) =>
      val leftStr = sanitizeQName(l)
      val rightStr = r match {
        case Left(i) => i.toString
        case Right(q) => sanitizeQName(q)
      }
      s"$leftStr $op $rightStr"
    case Condition.And(l, r) => s"(${conditionToString(l)}) && (${conditionToString(r)})"
    case Condition.Or(l, r) => s"(${conditionToString(l)}) || (${conditionToString(r)})"
  }

  private def statementToString(stmt: Statement): String = stmt match {
    case UpdateStmt(update) =>
      s"${sanitizeQName(update.variable)} = ${exprToString(update.expr)};"
    case IfThenStmt(condition, thenStmts) =>
      val thenBlock = thenStmts.map(statementToString).map("\t" + _).mkString("\n")
      s"if (${conditionToString(condition)}) {\n$thenBlock\n}"
  }

  private def stringToQName(str: String): QName = {
    if (str.isEmpty) Program2.QName(Nil)
    else Program2.QName(str.split('/').toList)
  }


  


  def convert(rxGraph: RxGraph, currentCode: String, layoutJson: String): String = {

    val layoutData = js.JSON.parse(layoutJson)
    val nodesPos = if (!js.isUndefined(layoutData.selectDynamic("nodes")) && layoutData.selectDynamic("nodes") != null) 
                 layoutData.selectDynamic("nodes") 
               else js.Dictionary.empty[js.Dynamic].asInstanceOf[js.Dynamic]

    val edgesPos = if (!js.isUndefined(layoutData.selectDynamic("edges")) && layoutData.selectDynamic("edges") != null) 
                    layoutData.selectDynamic("edges") 
                  else js.Dictionary.empty[js.Dynamic].asInstanceOf[js.Dynamic]
    

    def getPos(id: String): Point = {
      val p = nodesPos.selectDynamic(id)
      if (!js.isUndefined(p) && p != null) 
        Point(p.selectDynamic("x").asInstanceOf[Double], p.selectDynamic("y").asInstanceOf[Double])
      else 
        Point(0, 0)
    }

    def calculateNails(sourceId: String, targetId: String, edgeId: String): List[Point] = {
      val e = edgesPos.selectDynamic(edgeId)
      if (js.isUndefined(e) || e == null) return Nil
      
      val dists = e.selectDynamic("distances").asInstanceOf[js.Array[Double]].toList
      val weights = e.selectDynamic("weights").asInstanceOf[js.Array[Double]].toList
      
      val s = getPos(sourceId)
      val t = getPos(targetId)
      
      val dx = t.x - s.x
      val dy = t.y - s.y
      val length = Math.sqrt(dx*dx + dy*dy)
      if (length == 0) return Nil

      val nx = -dy / length
      val ny = dx / length

      dists.zip(weights).map { case (d, w) =>
        Point(
          s.x + w * dx + d * nx,
          s.y + w * dy + d * ny
        )
      }
    }

    

    val allStates = rxGraph.states.toList.sortBy(_.toString)
    val stateToId = allStates.zipWithIndex.map { case (qname, i) => qname -> s"id$i" }.toMap

    val actionLabels = rxGraph.edg.values.flatten.map(_._3).toSet.toList.sorted(Ordering.by[QName, String](_.toString))
    val labelToId: Map[QName, Int] = actionLabels.zipWithIndex.toMap

    val simpleEdges: List[Edge] = rxGraph.edg.flatMap { case (from, tos) =>
      tos.map { case (to, transId, lbl) => (from, to, transId, lbl) } 
    }.toList.distinct.sortBy(edge => (labelToId.getOrElse(edge._4, -1), edge._1.toString, edge._2.toString))
    val edgeToIndex: Map[Edge, Int] = simpleEdges.zipWithIndex.toMap

    type HyperEdgeIdentity = (String, QName, QName,QName, QName) // type, trigger, target, ruleName
    
    val ruleToLineNumber = {
      val ruleRegexFull = """^\s*([\w./]+)\s*(->>|--!)\s*([\w./]+)\s*:\s*([\w./]+).*""".r
      val ruleRegexShort = """^\s*([\w./]+)\s*(->>|--!)\s*([\w./]+).*""".r
      val lines = currentCode.linesIterator.zipWithIndex

      lines.flatMap { case (line, lineNumber) =>
        line.trim match {
          case ruleRegexFull(trigger, op, target, name) =>
            val opType = if (op == "->>") "on" else "off"
            val triggerQ = stringToQName(trigger)
            val targetQ = stringToQName(target)
            val nameQ = stringToQName(name)
            val key: HyperEdgeIdentity = (opType, triggerQ, targetQ, nameQ, nameQ)
            Some(key -> lineNumber)
            
          case ruleRegexShort(trigger, op, target) => 
            val opType = if (op == "->>") "on" else "off"
            val triggerQ = stringToQName(trigger)
            val targetQ = stringToQName(target)
            val key: HyperEdgeIdentity = (opType, triggerQ, targetQ, targetQ, targetQ)
            Some(key -> lineNumber)
            
          case _ => None
        }
      }.toMap
    }

    val hyperEdges = (
      rxGraph.on.flatMap  { case (trigger, targets) => targets.map(t => ("on",  trigger, t._1, t._2, t._3)) } ++
      rxGraph.off.flatMap { case (trigger, targets) => targets.map(t => ("off", trigger, t._1, t._2, t._3)) }
    ).toList.distinct
     .sortBy { h_identity => ruleToLineNumber.getOrElse(h_identity, Int.MaxValue) }

    val hyperEdgeToIndex: Map[HyperEdgeIdentity, Int] = hyperEdges.zipWithIndex.toMap
    val memo = mutable.Map[QName, Set[QName]]()

    def findAllRootTriggers(trigger: QName): Set[QName] = {
      if (memo.contains(trigger)) return memo(trigger)
      if (labelToId.contains(trigger)) return Set(trigger)
      val result = hyperEdges
        .filter { case (_, _, _, _, ruleName) => ruleName == trigger }
        .flatMap { case (_, parentTrigger, _, _, _) => findAllRootTriggers(parentTrigger) }
        .toSet
      memo(trigger) = result
      result
    }

    val arrayLInitializerEntries = hyperEdges.flatMap { hEdge =>
      val (opType, triggerLbl, targetLbl, selfId, selfLbl) = hEdge
      val rootTriggers = findAllRootTriggers(triggerLbl)
      val effectType = if (opType == "on") "1" else "0"

      val simpleEdgeTargets = simpleEdges
        .filter(_._4 == targetLbl)
        .map(e => (1, edgeToIndex(e))) 
      
      val hyperEdgeTargets = if (simpleEdgeTargets.nonEmpty) Nil else {
        hyperEdges
          .filter { case (_, _, _, _, ruleName) => ruleName == targetLbl }
          .flatMap { h_identity => hyperEdgeToIndex.get(h_identity).map(index => (0, index)) }
      }

      val status = if (rxGraph.act.contains((triggerLbl, targetLbl, selfId, selfLbl))) "1" else "0"
      val allTargets = simpleEdgeTargets ++ hyperEdgeTargets

      for {
        root <- rootTriggers
        rootId = labelToId.getOrElse(root, -1)
        (isEdgeTarget, targetIndex) <- allTargets
        if rootId != -1
      } yield {
        s"    { $rootId, $effectType, $status, $isEdgeTarget, $targetIndex } /* Rule '${selfLbl.show}' */"
      }
    }
    
    val finalNumHyperedges = {
      val s = arrayLInitializerEntries.distinct.size
      if (s == 0) 1 else s
    }


    
    val functionCounter = new AtomicInteger(0)
    val dataFunctions = new StringBuilder

    val clockDecl = if (rxGraph.clocks.nonEmpty) s"clock ${rxGraph.clocks.map(sanitizeQName).mkString(", ")};" else ""
    val varDecl = rxGraph.val_env.map { case (q, v) => s"int ${sanitizeQName(q)} = $v;" }.mkString("\n")

    val declarationBuilder = new StringBuilder(
      s"""// -----------------------------------------------------------
         |// 1. Variáveis e Clocks Globais
         |// -----------------------------------------------------------
         |$clockDecl
         |$varDecl
         |
         |// Constantes do Sistema
         |const int NUM_EDGES = ${simpleEdges.size};
         |const int NUM_HYPEREDGES = $finalNumHyperedges;
         |const int NUM_IDS = ${actionLabels.size};
         |""".stripMargin)

    declarationBuilder.append(
      """
        |// -----------------------------------------------------------
        |// 2. Definições de Estrutura Reativa
        |// -----------------------------------------------------------
        |typedef struct {
        |    int id;    // ID da ação
        |    bool stat; // Estado (1=ativo, 0=inativo)
        |} Edge;
        |
        |typedef struct {
        |    int id;    // ID da ação gatilho
        |    bool type; // Tipo de efeito (1=ativa, 0=desativa)
        |    bool stat; // Estado da regra
        |    bool is_edge_target; // 1 se alvo é Aresta, 0 se Regra
        |    int trg_index;       // Índice no array alvo
        |} Hyperedge;
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
         |Edge A[NUM_EDGES] = {
         |$arrayAInitializer
         |};
         |""".stripMargin)
    
    if (arrayLInitializerEntries.distinct.isEmpty) {
        declarationBuilder.append(s"Hyperedge L[NUM_HYPEREDGES];\n")
    } else {
        declarationBuilder.append(s"Hyperedge L[NUM_HYPEREDGES] = {\n${arrayLInitializerEntries.distinct.mkString(",\n")}\n};\n")
    }

    declarationBuilder.append(
      """
        |// -----------------------------------------------------------
        |// 4. Lógica de Atualização Reativa
        |// -----------------------------------------------------------
        |void update_hyperedges_by_id(int edge_id) {
        |    int i;
        |    for (i = 0; i < NUM_HYPEREDGES; i++) {
        |        if (L[i].id == edge_id && L[i].stat) { 
        |            if (L[i].is_edge_target) {
        |                A[L[i].trg_index].stat = L[i].type;
        |            } else {
        |                L[L[i].trg_index].stat = L[i].type;
        |            }
        |        }
        |    }
        |}
        |""".stripMargin)


    /*val HORIZONTAL_SPACING = 400
    val VERTICAL_SPACING = 350
    val numStates = allStates.size
    val columns = if (numStates > 0) Math.ceil(Math.sqrt(numStates)).toInt else 1

    val locationData = allStates.zipWithIndex.map { case (stateName, index) =>
      val row = index / columns
      val col = index % columns
      val x = col * HORIZONTAL_SPACING
      val y = row * VERTICAL_SPACING
      stateName -> (x, y)
    }.toMap
    */
    val locationNodes = allStates.map { stateName =>
      val stateId = stateToId(stateName)
      val pos = getPos(stateName.toString)//locationData(stateName)
      println(stateName.toString)
      println(pos)
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

        val labelX = actionNodePos.x.toInt
        val labelY = actionNodePos.y.toInt
        
        val reactiveGuard = s"A[$edgeIndex].stat == 1"
        val dataGuardOpt = rxGraph.edgeConditions.get(edge).flatten.map(conditionToString)
        
        val fullGuard = dataGuardOpt match {
            case Some(dg) => s"($reactiveGuard) && ($dg)"
            case None => reactiveGuard
        }

        val statements = rxGraph.edgeUpdates.getOrElse(edge, Nil)
        val dataUpdateCall = if (statements.nonEmpty) {
            val funcName = s"update_data_${functionCounter.getAndIncrement()}"
            val funcBody = statements.map(statementToString).mkString("\n\t")
            dataFunctions.append(s"void $funcName() {\n\t$funcBody\n}\n")
            s"$funcName(), "
        } else {
            ""
        }
        
        val fullAssignment = s"${dataUpdateCall}update_hyperedges_by_id($actionId)"


        <transition>
          <source ref={stateToId(source)}/>
          <target ref={stateToId(target)}/>
          <label kind="guard" x={(labelX - 40).toString} y={(labelY - 35).toString}>{fullGuard}</label>
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

    val nta =
      <nta>
        <declaration>{declarationBuilder.toString()}</declaration>
        <template>
          <name x="5" y="5">Template</name>
          {locationNodes}
          {initRef.map(ref => <init ref={ref}/>).getOrElse(NodeSeq.Empty)}
          {transitionNodes}
        </template>
        <system>Process = Template(); system Process;</system>
      </nta>

    val pp = new PrettyPrinter(200, 2)
    val formattedXml = pp.format(nta).replace("&amp;&amp;", "&&").replace("&&", "&amp;&amp;")

    val xmlString = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
                    "<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.6//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_6.dtd'>\n" +
                    formattedXml
                        
    xmlString.replace("  ", "\t")
  }
}