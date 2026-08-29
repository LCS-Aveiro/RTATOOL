package rta.backend

import rta.syntax.Program2
import rta.syntax.Program2.{Edge, QName, RxGraph}
import rta.syntax.{Condition, Statement, UpdateExpr, AssignStmt, ArrayAssignStmt, IfThenStmt, ForeachStmt, ReturnStmt, PrintStmt, RuntimeValue, FuncCallStmt, LocalDecl}
import java.util.concurrent.atomic.AtomicInteger
import scala.xml._
import scala.collection.mutable

object UppaalConverter3 {
  case class Point(x: Double, y: Double)
  case class EdgeLayout(distances: List[Double], weights: List[Double])

  private def sanitize(name: String): String = name.replaceAll("[^a-zA-Z0-9_]", "_")
  private def sanitizeQName(qname: QName): String = sanitize(qname.show)

  private def exprToString(expr: UpdateExpr): String = expr match {
    case UpdateExpr.LitInt(i)          => i.toString
    case UpdateExpr.LitFloat(f)        => f.toString
    case UpdateExpr.LitBool(b)         => if (b) "true" else "false"
    case UpdateExpr.LitArray(elems)    => "{" + elems.map(exprToString).mkString(", ") + "}"
    case UpdateExpr.Var(q)             => sanitizeQName(q)
    case UpdateExpr.ArrayAccess(arr, idx) => s"${sanitizeQName(arr)}[${exprToString(idx)}]"
    case UpdateExpr.MathOp(l, op, r)   => s"${exprToString(l)} $op ${exprToString(r)}"
    case UpdateExpr.FuncCall(f, args)  => s"${sanitizeQName(f)}(${args.map(exprToString).mkString(", ")})"
  }

  private def conditionToString(cond: Condition): String = cond match {
    case Condition.AtomicCond(l, op, r) => s"${exprToString(l)} $op ${exprToString(r)}"
    case Condition.And(l, r) => s"(${conditionToString(l)}) && (${conditionToString(r)})"
    case Condition.Or(l, r)  => s"(${conditionToString(l)}) || (${conditionToString(r)})"
  }

  // ------------------------------------------------------------------
  // Inferência de tipos: UPPAAL não aceita int = expressão real → fint()
  // ------------------------------------------------------------------
  private def exprIsFloat(expr: UpdateExpr, rx: RxGraph): Boolean = expr match {
    case UpdateExpr.LitFloat(_)      => true
    case UpdateExpr.LitInt(_)        => false
    case UpdateExpr.LitBool(_)       => false
    case UpdateExpr.LitArray(_)      => false
    case UpdateExpr.Var(q) =>
      rx.clocks.contains(q) || (rx.val_env.get(q) match {
        case Some(_: RuntimeValue.VFloat) => true
        case _                            => false
      })
    case UpdateExpr.ArrayAccess(arr, _) =>
      rx.val_env.get(arr) match {
        case Some(RuntimeValue.VArray(elems, _, _)) =>
          elems.headOption.exists(_.isInstanceOf[RuntimeValue.VFloat])
        case _ => false
      }
    case UpdateExpr.MathOp(l, _, r) => exprIsFloat(l, rx) || exprIsFloat(r, rx)
    case UpdateExpr.FuncCall(f, args) =>
      f.n.lastOption.getOrElse("") match {
        case "floor" | "ceil" | "round" | "fint"     => false
        case "sqrt" | "pow" | "random"               => true
        case "min" | "max" | "abs" | "mod" | "clamp" => args.exists(a => exprIsFloat(a, rx))
        case _ => rx.functions.get(f).exists(fd => returnsFloat(fd.body, rx))
      }
  }

  private def returnsFloat(stmts: List[Statement], rx: RxGraph): Boolean = stmts.exists {
    case ReturnStmt(e)        => exprIsFloat(e, rx)
    case IfThenStmt(_, thens) => returnsFloat(thens, rx)
    case _                    => false
  }

  private def targetIsInt(q: QName, rx: RxGraph): Boolean =
    !rx.clocks.contains(q) && (rx.val_env.get(q) match {
      case Some(_: RuntimeValue.VFloat) => false
      case Some(_: RuntimeValue.VBool)  => false
      case _                            => true
    })

  private def statementToString(stmt: Statement, rx: RxGraph): String = stmt match {
    case AssignStmt(variable, expr) =>
      val rhs  = exprToString(expr)
      val safe = if (targetIsInt(variable, rx) && exprIsFloat(expr, rx)) s"fint($rhs)" else rhs
      s"${sanitizeQName(variable)} = $safe;"
    case ArrayAssignStmt(arrName, index, expr) =>
      val rhs = exprToString(expr)
      val elemIsInt = rx.val_env.get(arrName) match {
        case Some(RuntimeValue.VArray(elems, _, _)) =>
          !elems.headOption.exists(_.isInstanceOf[RuntimeValue.VFloat])
        case _ => true
      }
      val safe = if (elemIsInt && exprIsFloat(expr, rx)) s"fint($rhs)" else rhs
      s"${sanitizeQName(arrName)}[${exprToString(index)}] = $safe;"
    case IfThenStmt(condition, thenStmts) =>
      val thenBlock = thenStmts.map(statementToString(_, rx)).map("\t" + _).mkString("\n")
      s"if (${conditionToString(condition)}) {\n$thenBlock\n}"
    case ForeachStmt(iter, arr, body) =>
      val bodyBlock = body.map(statementToString(_, rx)).map("\t" + _).mkString("\n")
      s"for (${sanitizeQName(iter)} : ${sanitizeQName(arr)}) {\n$bodyBlock\n}"
    case ReturnStmt(expr) => s"return ${exprToString(expr)};"
    case PrintStmt(_)     => "// print not supported in UPPAAL"
    case FuncCallStmt(funcName, args) =>
      s"${sanitizeQName(funcName)}(${args.map(exprToString).mkString(", ")});"
    case LocalDecl(typeName, variable, expr) =>
      val uppaalType = typeName match { case "float" => "double"; case other => other }
      val rhs  = exprToString(expr)
      val safe = if (typeName == "int" && exprIsFloat(expr, rx)) s"fint($rhs)" else rhs
      s"$uppaalType ${sanitizeQName(variable)} = $safe;"
  }

  // ------------------------------------------------------------------
  // Cronometro: detetar clocks lidos em posição de DADOS (ilegal no UPPAAL)
  // ------------------------------------------------------------------
  private def clockRefsInExpr(e: UpdateExpr, rx: RxGraph): Set[QName] = e match {
    case UpdateExpr.Var(q) if rx.clocks.contains(q) => Set(q)
    case UpdateExpr.MathOp(l, _, r)   => clockRefsInExpr(l, rx) ++ clockRefsInExpr(r, rx)
    case UpdateExpr.FuncCall(_, args) => args.flatMap(a => clockRefsInExpr(a, rx)).toSet
    case UpdateExpr.ArrayAccess(_, i) => clockRefsInExpr(i, rx)
    case UpdateExpr.LitArray(el)      => el.flatMap(e2 => clockRefsInExpr(e2, rx)).toSet
    case _ => Set.empty
  }
  private def clockRefsInCond(c: Condition, rx: RxGraph): Set[QName] = c match {
    case Condition.AtomicCond(l, _, r) => clockRefsInExpr(l, rx) ++ clockRefsInExpr(r, rx)
    case Condition.And(l, r) => clockRefsInCond(l, rx) ++ clockRefsInCond(r, rx)
    case Condition.Or(l, r)  => clockRefsInCond(l, rx) ++ clockRefsInCond(r, rx)
  }
  private def clockRefsInStmts(ss: List[Statement], rx: RxGraph): Set[QName] = ss.flatMap {
    case AssignStmt(_, e)         => clockRefsInExpr(e, rx)
    case ArrayAssignStmt(_, i, e) => clockRefsInExpr(i, rx) ++ clockRefsInExpr(e, rx)
    case IfThenStmt(c, th)        => clockRefsInCond(c, rx) ++ clockRefsInStmts(th, rx)
    case ForeachStmt(_, _, b)     => clockRefsInStmts(b, rx)
    case ReturnStmt(e)            => clockRefsInExpr(e, rx)
    case PrintStmt(e)             => clockRefsInExpr(e, rx)
    case FuncCallStmt(_, args)    => args.flatMap(a => clockRefsInExpr(a, rx)).toSet
    case LocalDecl(_, _, e)       => clockRefsInExpr(e, rx)
  }.toSet

  private def stringToQName(str: String): QName = {
    if (str.isEmpty) Program2.QName(Nil)
    else Program2.QName(str.split('/').toList)
  }

  def convert(rxGraph: RxGraph, currentCode: String, layout: UppaalLayout = EmptyLayout): String = {
    def getPos(id: String): Point = { val (x, y) = layout.getPos(id); Point(x, y) }
    def optPos(id: String): Option[Point] = if (layout.hasPos(id)) Some(getPos(id)) else None
    def calculateNails(sourceId: String, targetId: String, edgeId: String): List[Point] =
      layout.getNails(sourceId, targetId, edgeId).map(p => Point(p._1, p._2))

    val allStates = rxGraph.states.toList.sortBy(_.toString)
    val stateToId = allStates.zipWithIndex.map { case (qname, i) => qname -> s"id$i" }.toMap

    // ---- POSIÇÕES: layout do Cytoscape (ints); se faltar, grelha determinística ----
    val cols = 8
    val fallbackPos: Map[QName, Point] = allStates.zipWithIndex.map { case (s, idx) =>
      s -> Point((idx % cols) * 260.0, (idx / cols) * 180.0)
    }.toMap
    def statePos(s: QName): Point = optPos(s.toString).getOrElse(fallbackPos(s))
    def actionPos(actionNodeId: String, src: QName, dst: QName): Point =
      optPos(actionNodeId).getOrElse {
        val s = statePos(src); val t = statePos(dst)
        Point((s.x + t.x) / 2.0, (s.y + t.y) / 2.0 - 70.0)
      }

    val actionLabels = rxGraph.edg.values.flatten.map(_._3).toSet.toList.sorted(Ordering.by[QName, String](_.toString))
    val labelToId: Map[QName, Int] = actionLabels.zipWithIndex.toMap
    val simpleEdges: List[Edge] = rxGraph.edg.flatMap { case (from, tos) =>
      tos.map { case (to, transId, lbl) => (from, to, transId, lbl) }
    }.toList.distinct.sortBy(edge => (labelToId.getOrElse(edge._4, -1), edge._1.toString, edge._2.toString))
    val edgeToIndex: Map[Edge, Int] = simpleEdges.zipWithIndex.toMap

    // ---- CRONOMETRO: contador discreto global (nunca reseta) ----
    val clocksReadAsData: Set[QName] = {
      val fromEdges = rxGraph.edgeUpdates.values.flatMap(ss => clockRefsInStmts(ss.toList, rxGraph)).toSet
      val fromFuncs = rxGraph.functions.values.flatMap(f => clockRefsInStmts(f.body, rxGraph)).toSet
      fromEdges ++ fromFuncs
    }
    val useCrono = clocksReadAsData.nonEmpty
    val CRONO = "delay_counter"
    def substClocks(e: UpdateExpr): UpdateExpr = e match {
      case UpdateExpr.Var(q) if clocksReadAsData.contains(q) => UpdateExpr.Var(QName(List(CRONO)))
      case UpdateExpr.MathOp(l, op, r)  => UpdateExpr.MathOp(substClocks(l), op, substClocks(r))
      case UpdateExpr.FuncCall(f, args) => UpdateExpr.FuncCall(f, args.map(substClocks))
      case UpdateExpr.ArrayAccess(a, i) => UpdateExpr.ArrayAccess(a, substClocks(i))
      case UpdateExpr.LitArray(el)      => UpdateExpr.LitArray(el.map(substClocks))
      case other => other
    }
    def substCond(c: Condition): Condition = c match {
      case Condition.AtomicCond(l, op, r) => Condition.AtomicCond(substClocks(l), op, substClocks(r))
      case Condition.And(l, r) => Condition.And(substCond(l), substCond(r))
      case Condition.Or(l, r)  => Condition.Or(substCond(l), substCond(r))
    }
    def transformStmts(ss: List[Statement]): List[Statement] = ss.flatMap { s =>
      val t: Statement = s match {
        case AssignStmt(v, e)         => AssignStmt(v, substClocks(e))
        case ArrayAssignStmt(a, i, e) => ArrayAssignStmt(a, substClocks(i), substClocks(e))
        case IfThenStmt(c, th)        => IfThenStmt(substCond(c), transformStmts(th))
        case ForeachStmt(it, a, b)    => ForeachStmt(it, a, transformStmts(b))
        case ReturnStmt(e)            => ReturnStmt(substClocks(e))
        case PrintStmt(e)             => PrintStmt(substClocks(e))
        case FuncCallStmt(f, args)    => FuncCallStmt(f, args.map(substClocks))
        case LocalDecl(tp, v, e)      => LocalDecl(tp, v, substClocks(e))
      }
      t match {
        case AssignStmt(v, UpdateExpr.LitInt(0)) if rxGraph.clocks.contains(v) && clocksReadAsData.contains(v) =>
          List(t, AssignStmt(QName(List(CRONO)), UpdateExpr.LitInt(0)))
        case AssignStmt(v, UpdateExpr.LitFloat(f)) if f == 0.0 && rxGraph.clocks.contains(v) && clocksReadAsData.contains(v) =>
          List(t, AssignStmt(QName(List(CRONO)), UpdateExpr.LitInt(0)))
        case _ => List(t)
      }
    }

    type HyperEdgeIdentity = (String, QName, QName, QName, QName)
    val ruleToLineNumber = {
      val ruleRegexFull  = """^\s*([\w./]+)\s*(->>|--!)\s*([\w./]+)\s*:\s*([\w./]+).*""".r
      val ruleRegexShort = """^\s*([\w./]+)\s*(->>|--!)\s*([\w./]+).*""".r
      val lines = currentCode.linesIterator.zipWithIndex
      lines.flatMap { case (line, lineNumber) =>
        line.trim match {
          case ruleRegexFull(trigger, op, target, name) =>
            val opType = if (op == "->>") "on" else "off"
            val triggerQ = stringToQName(trigger); val targetQ = stringToQName(target); val nameQ = stringToQName(name)
            val key: HyperEdgeIdentity = (opType, triggerQ, targetQ, nameQ, nameQ)
            Some(key -> lineNumber)
          case ruleRegexShort(trigger, op, target) =>
            val opType = if (op == "->>") "on" else "off"
            val triggerQ = stringToQName(trigger); val targetQ = stringToQName(target)
            val key: HyperEdgeIdentity = (opType, triggerQ, targetQ, targetQ, targetQ)
            Some(key -> lineNumber)
          case _ => None
        }
      }.toMap
    }
    val hyperEdges = (
      rxGraph.on.flatMap  { case (trigger, targets) => targets.map(t => ("on",  trigger, t._1, t._2, t._3)) } ++
      rxGraph.off.flatMap { case (trigger, targets) => targets.map(t => ("off", trigger, t._1, t._2, t._3)) }
    ).toList.distinct.sortBy(h => ruleToLineNumber.getOrElse(h, Int.MaxValue))
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
      val simpleEdgeTargets = simpleEdges.filter(_._4 == targetLbl).map(e => (1, edgeToIndex(e)))
      val hyperEdgeTargets = if (simpleEdgeTargets.nonEmpty) Nil else {
        hyperEdges
          .filter { case (_, _, _, _, ruleName) => ruleName == targetLbl }
          .flatMap(h => hyperEdgeToIndex.get(h).map(index => (0, index)))
      }
      val status = if (rxGraph.act.contains((triggerLbl, targetLbl, selfId, selfLbl))) "1" else "0"
      val allTargets = simpleEdgeTargets ++ hyperEdgeTargets
      for {
        root <- rootTriggers
        rootId = labelToId.getOrElse(root, -1)
        (isEdgeTarget, targetIndex) <- allTargets
        if rootId != -1
      } yield s"    { $rootId, $effectType, $status, $isEdgeTarget, $targetIndex } /* Rule '${selfLbl.show}' */"
    }
    val finalNumHyperedges = { val s = arrayLInitializerEntries.distinct.size; if (s == 0) 1 else s }

    // ---- Funções de dados geradas (com DEDUP por corpo idêntico) ----
    val functionCounter = new AtomicInteger(0)
    val dataFunctions = new StringBuilder
    val bodyToFuncName = mutable.Map[String, String]()

    val clockDecl = if (rxGraph.clocks.nonEmpty) s"clock ${rxGraph.clocks.map(sanitizeQName).mkString(", ")};" else ""
    val cronoDecl = if (useCrono) s"int $CRONO = 0;" else ""
    val varDecl = rxGraph.val_env.map { case (q, v) =>
      val typeStr = v match {
        case _: RuntimeValue.VBool  => "bool"
        case _: RuntimeValue.VFloat => "double"
        case _                      => "int"
      }
      val valStr = v match {
        case RuntimeValue.VArray(elems, _, _) => "{" + elems.map(_.value).mkString(", ") + "}"
        case _ => v.value.toString
      }
      val arrBrackets = v match {
        case RuntimeValue.VArray(_, _, maxOpt) => s"[${maxOpt.getOrElse(100)}]"
        case _ => ""
      }
      s"$typeStr ${sanitizeQName(q)}$arrBrackets = $valStr;"
    }.mkString("\n")

    def getReturnType(stmts: List[Statement]): String = {
      if (stmts.exists {
        case _: ReturnStmt => true
        case IfThenStmt(_, thens)   => getReturnType(thens) == "int"
        case ForeachStmt(_, _, body) => getReturnType(body) == "int"
        case _ => false
      }) "int" else "void"
    }

    val customFuncs = rxGraph.functions.values.map { f =>
      val params  = f.params.map(p => s"int ${sanitizeQName(p)}").mkString(", ")
      val retType = getReturnType(f.body)
      val body    = transformStmts(f.body).map(statementToString(_, rxGraph)).map("\t" + _).mkString("\n")
      s"$retType ${sanitizeQName(f.name)}($params) {\n$body\n}"
    }.mkString("\n")

    val declarationBuilder = new StringBuilder(
      s"""// -----------------------------------------------------------
         |// 1. Variáveis e Clocks Globais
         |// -----------------------------------------------------------
         |$clockDecl
         |$cronoDecl
         |$varDecl
         |$customFuncs
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

    // ---- Locations (coordenadas INT + fallback) ----
    val locationNodes = allStates.map { stateName =>
      val stateId = stateToId(stateName)
      val pos = statePos(stateName)
      val px = Math.round(pos.x).toInt
      val py = Math.round(pos.y).toInt
      val invariantNode = rxGraph.invariants.get(stateName)
        .map(cond => <label kind="invariant" x={px.toString} y={(py + 15).toString}>{conditionToString(cond)}</label>)
        .getOrElse(NodeSeq.Empty)
      <location id={stateId} x={px.toString} y={py.toString}>
        <name x={(px - 20).toString} y={(py - 30).toString}>{sanitizeQName(stateName)}</name>
        {invariantNode}
      </location>
    }

    // ---- Transitions (nails INT + ação como waypoint + dedup de updates) ----
    val transitionNodes = simpleEdges.map { edge =>
      val (source, target, transId, lbl) = edge
      val edgeIndex = edgeToIndex(edge)
      val actionId  = labelToId.getOrElse(lbl, -1)
      val actionNodeId = s"event_${source}_${target}_${transId}_${lbl}"
      val cyEdge1Id = s"s_to_a_${source}_${actionNodeId}"
      val cyEdge2Id = s"a_to_s_${actionNodeId}_${target}"
      val aPos   = actionPos(actionNodeId, source, target)
      val nails1 = calculateNails(source.toString, actionNodeId, cyEdge1Id)
      val nails2 = calculateNails(actionNodeId, target.toString, cyEdge2Id)
      val allNails = (nails1 :+ aPos) ++ nails2
      val labelX = Math.round(aPos.x).toInt
      val labelY = Math.round(aPos.y).toInt
      val reactiveGuard = s"A[$edgeIndex].stat == 1"
      val dataGuardOpt = rxGraph.edgeConditions.get(edge).flatten.map(conditionToString)
      val fullGuard = dataGuardOpt match {
        case Some(dg) => s"($reactiveGuard) && ($dg)"
        case None     => reactiveGuard
      }
      val statements = transformStmts(rxGraph.edgeUpdates.getOrElse(edge, Nil))
      val dataUpdateCall = if (statements.nonEmpty) {
        val funcBody = statements.map(statementToString(_, rxGraph)).mkString("\n\t")
        val funcName = bodyToFuncName.getOrElseUpdate(funcBody, {
          val n = s"update_data_${functionCounter.getAndIncrement()}"
          dataFunctions.append(s"void $n() {\n\t$funcBody\n}\n")
          n
        })
        s"$funcName(), "
      } else ""
      val fullAssignment = s"${dataUpdateCall}update_hyperedges_by_id($actionId)"
      <transition>
        <source ref={stateToId(source)}/>
        <target ref={stateToId(target)}/>
        <label kind="guard" x={(labelX - 40).toString} y={(labelY - 35).toString}>{fullGuard}</label>
        <label kind="assignment" x={(labelX - 40).toString} y={(labelY + 15).toString}>{fullAssignment}</label>
        {allNails.map { p => <nail x={Math.round(p.x).toInt.toString} y={Math.round(p.y).toInt.toString}/> }}
      </transition>
    }

    declarationBuilder.append(
      s"""
         |// -----------------------------------------------------------
         |// 5. Funções de Dados (Geradas)
         |// -----------------------------------------------------------
         |${dataFunctions.toString()}
         |""".stripMargin)

    // ---- Template Cronometro (contador discreto, soma +1 até ao fim, SEM reset) ----
    val cronometroTemplate = if (!useCrono) NodeSeq.Empty else {
      <template>
        <name x="5" y="5">Cronometro</name>
        <declaration>clock tick;</declaration>
        <location id="crono_tick" x="0" y="0">
          <name x="-10" y="-35">Tick</name>
          <label kind="invariant" x="-10" y="15">tick &lt;= 1</label>
        </location>
        <init ref="crono_tick"/>
        <transition>
          <source ref="crono_tick"/>
          <target ref="crono_tick"/>
          <label kind="guard" x="60" y="-25">tick == 1</label>
          <label kind="assignment" x="60" y="10">delay_counter++, tick = 0</label>
          <nail x="70" y="-45"/>
          <nail x="70" y="45"/>
        </transition>
      </template>
    }

    val initRef = rxGraph.inits.headOption.flatMap(stateToId.get)
    val systemStr =
      s"""Process = Template();
         |${if (useCrono) "Crono = Cronometro();\n" else ""}system Process${if (useCrono) ", Crono" else ""};""".stripMargin

    val nta =
      <nta>
        <declaration>{declarationBuilder.toString()}</declaration>
        <template>
          <name x="5" y="5">Template</name>
          {locationNodes}
          {initRef.map(ref => <init ref={ref}/>).getOrElse(NodeSeq.Empty)}
          {transitionNodes}
        </template>
        {cronometroTemplate}
        <system>{systemStr}</system>
      </nta>

    val pp = new PrettyPrinter(200, 2)
    val formattedXml = pp.format(nta).replace("&amp;&amp;", "&&").replace("&&", "&amp;&amp;")
    val xmlString = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
      "<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.6//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_6.dtd'>\n" +
      formattedXml
    xmlString.replace("  ", "\t")
  }
}