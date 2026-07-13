package rta.frontend

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import rta.syntax.Parser2
import rta.syntax.Program2.{RxGraph, Edge, QName}
import rta.backend.{RxSemantics, CytoscapeConverter, PdlEvaluator, MCRL2, UppaalConverter3,UppaalConverter4, AnalyseLTS}
import rta.syntax.PdlParser
import rta.syntax.RTATranslator
import rta.syntax.Condition
import rta.backend.{LtlEvaluator, TraceGenerator}
import rta.syntax.{LtlParser, PdlParser}
import rta.backend.AnalyseLTS.ZoneStateKey
import rta.backend.{UppaalLayout, EmptyLayout}
import rta.syntax.{CtlFormula, CtlParser}
import rta.backend.CtlEvaluator

@JSExportTopLevel("RTA")
object RTAAPI {

  private var currentGraph: Option[RxGraph] = None
  private var currentSource: String = ""
  private var history: List[RxGraph] = Nil



  class JSLayout(layoutJson: String) extends UppaalLayout {
    val data = try { js.JSON.parse(layoutJson) } catch { case _: Throwable => js.Dynamic.literal() }
    val nodesPos = if (!js.isUndefined(data.selectDynamic("nodes")) && data.selectDynamic("nodes") != null) data.selectDynamic("nodes") else js.Dynamic.literal()
    val edgesPos = if (!js.isUndefined(data.selectDynamic("edges")) && data.selectDynamic("edges") != null) data.selectDynamic("edges") else js.Dynamic.literal()

    def getPos(id: String): (Double, Double) = {
      val p = nodesPos.selectDynamic(id)
      if (!js.isUndefined(p) && p != null) (p.selectDynamic("x").asInstanceOf[Double], p.selectDynamic("y").asInstanceOf[Double])
      else (0.0, 0.0)
    }

    def getNails(sourceId: String, targetId: String, edgeId: String): List[(Double, Double)] = {
      val e = edgesPos.selectDynamic(edgeId)
      if (js.isUndefined(e) || e == null) return Nil
      val dists = e.selectDynamic("distances").asInstanceOf[js.Array[Double]].toList
      val weights = e.selectDynamic("weights").asInstanceOf[js.Array[Double]].toList
      val s = getPos(sourceId)
      val t = getPos(targetId)
      val dx = t._1 - s._1
      val dy = t._2 - s._2
      val length = Math.sqrt(dx*dx + dy*dy)
      if (length == 0) return Nil
      val nx = -dy / length
      val ny = dx / length
      dists.zip(weights).map { case (d, w) =>
        (s._1 + w * dx + d * nx, s._2 + w * dy + d * ny)
      }
    }
  }

  private def escapeJson(str: String): String = {
    if (str == null) "" else str
      .replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
      .replace("\r", "")
      .replace("\t", "\\t")
  }


  
  @JSExport
  def findBestPath(targetStr: String): String = {
    currentGraph match {
      case Some(rx) =>
        val adaptedTarget = targetStr.replace('/', '.')
        Parser2.pp[QName](Parser2.qname, adaptedTarget) match {
          case Right(targetQName) =>
            AnalyseLTS.findShortestPath(rx, targetQName) match {
              case Some(steps) => 
                js.JSON.stringify(js.Array(steps: _*))
              case None => 
                """{"error": "Caminho não encontrado ou muito longo."}"""
            }
          case Left(err) => 
            s"""{"error": "Estado inválido: $err"}"""
        }
      case None => 
        """{"error": "Carregue o modelo primeiro."}"""
    }
  }

  @JSExport
  def findPathToValue(condStr: String): js.Any = {
    currentGraph match {
      case Some(rx) =>
        try {
          val formatted = condStr
            .replace("&&", "] &&[")
            .replace("AND", "] && [")
          
          val formulaStr = s"[$formatted]"
          val formula = PdlParser.parsePdlFormula(formulaStr)
          
          def formulaToCondition(f: rta.syntax.Formula): rta.syntax.Condition = f match {
            case rta.syntax.Formula.CondProp(c) => c
            case rta.syntax.Formula.And(f1, f2) => rta.syntax.Condition.And(formulaToCondition(f1), formulaToCondition(f2))
            case _ => throw new Exception("Use apenas comparações simples unidas por && ou AND")
          }

          val finalCond = formulaToCondition(formula)

          AnalyseLTS.findShortestPathToCondition(rx, finalCond) match {
            case Some(steps) => js.Array(steps: _*)
            case None => js.Dynamic.literal(error = "Caminho não encontrado ou inatingível.")
          }
        } catch {
          case e: Throwable => js.Dynamic.literal(error = s"Erro: ${e.getMessage}")
        }
      case None => js.Dynamic.literal(error = "Modelo não carregado.")
    }
  }


  @JSExport
  def getAllStepsMermaid(): String = {
    currentGraph.map { root =>
      var visited = Set[RxGraph](root)
      var queue = List(root)
      var transitionsStr = List[String]()
      
      var stateToId = Map[RxGraph, Int](root -> 0)
      var idCounter = 0
      
      def getId(g: RxGraph): Int = {
        stateToId.getOrElse(g, {
          idCounter += 1
          stateToId += (g -> idCounter)
          idCounter
        })
      }

      val maxStates = 500 

      while(queue.nonEmpty && visited.size < maxStates) {
        val current = queue.head
        queue = queue.tail
        val sourceId = getId(current)
        
        val edgeNexts = RxSemantics.nextEdge(current)
        for ((edge, nextState) <- edgeNexts) {
          val (from, to, tId, label) = edge
          val targetId = getId(nextState)
          
          val displayLabel = if (tId == label) label.show else s"${label.show}(${tId.show})"
          
          transitionsStr = s"""$sourceId --->|"$displayLabel"| $targetId""" :: transitionsStr
          if (!visited.contains(nextState)) {
            visited += nextState
            queue = queue :+ nextState
          }
        }

        if (current.clocks.nonEmpty) {
          val delayedClockEnv = current.clock_env.map { case (c, v) => (c, v + 1.0) }
          val nextTimeState = current.copy(clock_env = delayedClockEnv)
          
          val canTimePass = current.inits.forall(s => 
            nextTimeState.invariants.get(s) match {
              case Some(inv) => RxSemantics.evalCondition(inv, nextTimeState)
              case None => true
            }
          )

          if (canTimePass) {
            val targetId = getId(nextTimeState)
            transitionsStr = s"""$sourceId --->|"delay(1)"| $targetId""" :: transitionsStr
            if (!visited.contains(nextTimeState)) {
              visited += nextTimeState
              queue = queue :+ nextTimeState
            }
          }
        }
      }
       
      val nodeDefinitions = stateToId.map { case (state, id) =>
        val clockValues = state.clock_env.map(kv => s"${kv._1.show}=${kv._2.toInt}").mkString(", ")
        val label = s"${state.inits.mkString(", ")} <br/>($clockValues)"
        val style = if (state == root) s"\nstyle $id fill:#9ece6a,stroke:#333,stroke-width:2px" else ""
        s"""$id("$label")$style"""
      }.mkString("\n")

      s"""graph LR
         |${transitionsStr.distinct.reverse.mkString("\n")}
         |$nodeDefinitions
         |""".stripMargin
      
    }.getOrElse("graph LR\n0(Nenhum modelo carregado)")
  }

  @JSExport
  def loadModel(sourceCode: String): String = {
    try {
      currentSource = sourceCode
      val parsedGraph = Parser2.parseProgram(sourceCode)
      val maxC = RxSemantics.MaxConstants.compute(parsedGraph)
      val graph = parsedGraph.copy(maxConstants = maxC)
      
      currentGraph = Some(graph)
      history = List(graph)
      generateSimulationJson(graph, None)
    } catch {
      case e: Throwable =>
        s"""{"error": "${escapeJson("Error parsing: " + e.getMessage)}"}"""
    }
  }

  @JSExport
  def takeStep(edgeJson: String): String = {
    currentGraph match {
      case Some(graph) =>
        try {
          val edgeData = js.JSON.parse(edgeJson)
          val from = stringToQName(edgeData.selectDynamic("from").toString)
          val to = stringToQName(edgeData.selectDynamic("to").toString)
          
          val tId = stringToQName(edgeData.selectDynamic("tId").toString)
          val label = stringToQName(edgeData.selectDynamic("label").toString)
          
          val clickedEdge: Edge = (from, to, tId, label)

          RxSemantics.nextEdge(graph).find(_._1 == clickedEdge) match {
            case Some((_, nextGraph)) =>
              history = nextGraph :: history
              currentGraph = Some(nextGraph)
              generateSimulationJson(nextGraph, Some(clickedEdge))
            case None => s"""{"error": "Transição inválida."}"""
          }
        } catch {
          case e: Throwable => s"""{"error": "${escapeJson(e.getMessage)}"}"""
        }
      case None => """{"error": "Nenhum modelo carregado."}"""
    }
  }

  @JSExport
  def undo(): String = {
    if (history.size > 1) {
      history = history.tail
      currentGraph = history.headOption
      generateSimulationJson(currentGraph.get, None)
    } else {
      currentGraph.map(g => generateSimulationJson(g, None)).getOrElse("{}")
    }
  }


  private def runtimeValueToJson(v: rta.syntax.RuntimeValue): String = v match {
    case rta.syntax.RuntimeValue.VInt(i, _, _) => i.toString
    case rta.syntax.RuntimeValue.VFloat(f, _, _) => f.toString
    case rta.syntax.RuntimeValue.VBool(b) => b.toString
    case rta.syntax.RuntimeValue.VArray(elems, _, _) => "[" + elems.map(runtimeValueToJson).mkString(", ") + "]"
  }

  @JSExport
  def advanceTime(delayAmount: Double): String = {
    currentGraph match {
      case Some(currentState) =>
        if (currentState.clocks.isEmpty || delayAmount <= 0) {
          generateSimulationJson(currentState, None)
        } else {
          val delayedClockEnv = currentState.clock_env.map { case (c, v) => (c, v + delayAmount) }
          val stateWithTime = currentState.copy(clock_env = delayedClockEnv)

          val potentialNextState = RxSemantics.applyTimeouts(stateWithTime)

          val allInvariantsHold = potentialNextState.inits.forall { s =>
            potentialNextState.invariants.get(s) match {
              
              case Some(inv) => RxSemantics.evalCondition(inv, potentialNextState)
              case None => true
            }
          }

          if (allInvariantsHold) {
            history = potentialNextState :: history
            currentGraph = Some(potentialNextState)
            generateSimulationJson(potentialNextState, None)
          } else {
            generateSimulationJson(currentState, None)
          }
        }
      case None => "{}"
    }
  }


  

  @JSExport
  def getMcrl2(): String = currentGraph.map(g => MCRL2(g)).getOrElse("Modelo vazio")

  @JSExport
  def translateToGLTS(): String = {
    currentGraph match {
      case Some(g) => RTATranslator.translate_syntax(g, currentSource)
      case None => "Error: Please load a model first."
    }
  }

  
  @JSExport
  def getUppaalTGRG(layoutJson: String): String = currentGraph.map(g => UppaalConverter3.convert(g, currentSource, new JSLayout(layoutJson))).getOrElse("")


  @JSExport
  def getUppaal(layoutJson: String): String = currentGraph.map(g => UppaalConverter4.convert(g, currentSource, layoutJson)).getOrElse("")

  @JSExport
  def checkProblems(): String = {
    currentGraph.map { g =>
      AnalyseLTS.randomWalk(g)._4 match {
        case Nil => "Nenhum problema encontrado."
        case m => m.mkString("\n")
      }
    }.getOrElse("Modelo vazio")
  }

  @JSExport
  def getStats(): String = {
    currentGraph.map { root =>
      var visited = Set[RxGraph]()
      var toVisit = List(root)
      var edgesCount = 0
      val limit = 2000
      
      while(toVisit.nonEmpty && visited.size < limit) {
        val current = toVisit.head
        toVisit = toVisit.tail
        if (!visited.contains(current)) {
           visited += current
           val nexts = RxSemantics.nextEdge(current).map(_._2)
           edgesCount += nexts.size
           toVisit = toVisit ++ nexts.toList
        }
      }
      val msg = if (visited.size >= limit) s" (parou após $limit estados)" else ""
      s"""== Estatísticas ==\nEstados: ${visited.size}$msg\nTransições: $edgesCount"""
    }.getOrElse("Modelo vazio")
  }


  @JSExport
  def runPdl(stateStr: String, formulaStr: String, traceLength: Int): String = {
    currentGraph match {
      case Some(rx) =>
        try {
          val adaptedState = stateStr.replace('/', '.')
          Parser2.pp[QName](Parser2.qname, adaptedState) match {
            case Left(err) => s"""{"error": "${escapeJson(s"Error parsing state '$stateStr': $err")}"}"""
            case Right(startState) =>
              if (!rx.states.contains(startState)) {
                s"""{"error": "${escapeJson(s"State '${startState.show}' not found in the current model.")}"}"""
              } else if (LtlParser.looksLikeLtl(formulaStr)) {
                val formula = LtlParser.parseLtlFormula(formulaStr)
                val startGraph = rx.copy(inits = Set(startState))
                val (trace, pathLabels, edgeIds) = TraceGenerator.randomTraceDetailed(startGraph, traceLength)
                val result = LtlEvaluator.eval(trace, formula, LtlEvaluator.Hybrid)

                val traceStr = if (pathLabels.isEmpty) "Nenhum traço gerado (deadlock ou vazio)" else pathLabels.mkString(" ➔ ")
                val msg = s"Result: $result\n[LTL Trace: $traceLength steps]\nStart ➔ $traceStr"
                val pathJson = js.JSON.stringify(js.Array(edgeIds: _*))
                s"""{"result": $result, "text": "${escapeJson(msg)}", "path": $pathJson}"""
              } else {
                val formula = PdlParser.parsePdlFormula(formulaStr)
                val result = PdlEvaluator.evaluateFormula(startState, formula, rx)
                val msg = s"Result: $result\n(Standard PDL Evaluation / Trace Not Used)"
                s"""{"result": $result, "text": "${escapeJson(msg)}", "path": []}"""
              }
          }
        } catch {
          case e: Throwable =>
            val msg = if (e.getMessage != null) e.getMessage else e.toString
            s"""{"error": "${escapeJson(s"Evaluation Error: $msg")}"}"""
        }
      case None => """{"error": "Model not loaded."}"""
    }
  }




  @JSExport
  def findBestPathZone(targetStr: String): String = {
    currentGraph match {
      case Some(rx) =>
        val adaptedTarget = targetStr.replace('/', '.')
        Parser2.pp[QName](Parser2.qname, adaptedTarget) match {
          case Right(targetQName) =>
            AnalyseLTS.findShortestPath(rx, targetQName, maxStates = 5000, useZones = true) match {
              case Some(steps) => 
                js.JSON.stringify(js.Array(steps: _*))
              case None => 
                """{"error": "Caminho não encontrado (Motor Simbólico / DBM)."}"""
            }
          case Left(err) => 
            s"""{"error": "Estado inválido: $err"}"""
        }
      case None => 
        """{"error": "Carregue o modelo primeiro."}"""
    }
  }

  @JSExport
  def runLTLExhaustive(formulaStr: String, maxStates: Int, maxDepth: Int): String = {
    currentGraph match {
      case Some(startGraph) =>
        try {
          val formula = LtlParser.parseLtlFormula(formulaStr)

          val queryConstants = RxSemantics.MaxConstants.fromLTL(formula, startGraph.clocks)
          val boostedStartGraph = startGraph.copy(
            maxConstants = RxSemantics.MaxConstants.mergeMax(startGraph.maxConstants, queryConstants)
          )

          
          val (success, ceLabels, ceIds, explored, limitReached, visitLog) = 
              AnalyseLTS.verifyLTLSymbolic(boostedStartGraph, formula, maxStates, maxDepth)

          val ceStr = if (ceLabels.isEmpty) "Nenhum (Falha no 1º estado)" else ceLabels.mkString(" ➔ ")
          val pathJson = js.JSON.stringify(js.Array(ceIds: _*))
          val visitLogJson = js.JSON.stringify(js.Array(visitLog: _*)) 

          s"""{"success": $success, "explored": $explored, "limitReached": $limitReached, "counterExample": "${escapeJson(ceStr)}", "path": $pathJson, "visitLog": $visitLogJson}"""
        } catch {
          case e: Throwable => s"""{"error": "${escapeJson(s"Erro na Prova DBM: ${e.getMessage}")}"}"""
        }
      case None => """{"error": "Modelo não carregado."}"""
    }
  }

  def getEdgeStatus(rx: RxGraph): (String, String) = {
    val allEdges: Set[Edge] = 
      rx.edg.flatMap { case (f, ts) => ts.map(t => (f, t._1, t._2, t._3)) }.toSet ++
      rx.on.flatMap  { case (f, ts) => ts.map(t => (f, t._1, t._2, t._3)) }.toSet ++
      rx.off.flatMap { case (f, ts) => ts.map(t => (f, t._1, t._2, t._3)) }.toSet
      
    val active = rx.act
    val inactive = allEdges -- active
    
    val activeStr = active.map(_._4.show).toList.sorted.mkString(", ")
    val inactiveStr = inactive.map(_._4.show).toList.sorted.mkString(", ")
    
    (activeStr, inactiveStr)
  }


  @JSExport
  def runCTLExhaustive(formulaStr: String, maxStates: Int): String = {
    currentGraph match {
      case Some(startGraph) =>
        try {
          val formula = CtlParser.parseCtlFormula(formulaStr)

          // Impulso do Limite de Extrapolação de Relógios (DBM MaxConstants) baseado nas Variáveis lidas na Fórmula
          val queryConstants = CtlEvaluator.getConstants(formula, startGraph.clocks)
          val boostedStartGraph = startGraph.copy(
            maxConstants = rta.backend.RxSemantics.MaxConstants.mergeMax(startGraph.maxConstants, queryConstants)
          )

          val (success, explored, ceLabels, ceIds) = CtlEvaluator.verifyCTLSymbolic(boostedStartGraph, formula, maxStates)

          val ceStr = if (ceLabels.isEmpty) "Omissão (Provas CTL em árvore nem sempre produzem traços lineares diretos, ou a sua query não gerou nenhum.)" else ceLabels.mkString(" ➔ ")
          val pathJson = js.JSON.stringify(js.Array(ceIds: _*))

          s"""{"success": $success, "explored": $explored, "counterExample": "${escapeJson(ceStr)}", "path": $pathJson}"""
        } catch {
          case e: Throwable => s"""{"error": "${escapeJson(s"Erro na Prova CTL: ${e.getMessage}")}"}"""
        }
      case None => """{"error": "Modelo não carregado."}"""
    }
  }


  @JSExport
  def getSymbolicStepsJSON(): String = {
    import rta.backend.AnalyseLTS.ZoneStateKey

    def sanitizeLabel(s: String): String = {
      s.replace("<=", "≤").replace(">=", "≥").replace("<", "⋖").replace(">", "⋗")
       .replace("\\", "\\\\").replace("\"", "\\\"")
    }

    currentGraph.map { root =>
      var visited = Set[ZoneStateKey]()
      var queue = List(root)
      var elements = collection.mutable.ListBuffer[String]()
      
      var stateToId = Map[ZoneStateKey, String]()
      var idCounter = 0
      
      def getId(g: RxGraph): String = {
        val key = ZoneStateKey(g.inits, g.val_env, g.zone, g.pendingDelays,g.act)
        stateToId.getOrElse(key, {
          val newId = s"z$idCounter"
          idCounter += 1
          stateToId += (key -> newId)
          newId
        })
      }
      
      val startKey = ZoneStateKey(root.inits, root.val_env, root.zone, root.pendingDelays,root.act)
      val startId = getId(root)
      visited += startKey

      def buildNodeJson(g: RxGraph, id: String): String = {
         val key = ZoneStateKey(g.inits, g.val_env, g.zone, g.pendingDelays, g.act)
         

         val displayZone = rta.backend.RxSemantics.advanceTimeZone(g).map(_.zone).getOrElse(key.zone)
         
         val zoneStr = sanitizeLabel(showZoneCompact(displayZone))
         val varsStr = sanitizeLabel(key.vars.filterNot(_._1.n.contains("__return")).map(kv => s"${kv._1.show}=${kv._2.value}").mkString(", "))
         val pendingStr = sanitizeLabel(key.pending.map { case (edge, op, clock, target) =>
            s"${edge._4.show}(${op})@${clock.show}=${target}"
         }.mkString(", "))
         
         val labelParts = List(
            s"ESTADOS: ${key.inits.mkString(", ")}",
            if (zoneStr.nonEmpty) s"ZONA: $zoneStr" else "ZONA: t ≥ 0",
            if (varsStr.nonEmpty) s"VARS: $varsStr" else "",
            if (pendingStr.nonEmpty) s"PENDENTE: $pendingStr" else ""
         ).filter(_.nonEmpty)
         
         val label = labelParts.mkString("\\n")
         val isStart = if (id == "z0") "true" else "false"

         val (actStr, inactStr) = getEdgeStatus(g)
         val safeAct = sanitizeLabel(actStr)
         val safeInact = sanitizeLabel(inactStr)
         
         s"""{"data": {"id": "$id", "label": "$label", "isStart": $isStart, "actEdges": "$safeAct", "inactEdges": "$safeInact"}}"""
      }
      
      elements += buildNodeJson(root, startId)

      val maxStates = 300
      var edgeCounter = 0

      while(queue.nonEmpty && visited.size < maxStates) {
        val current = queue.head
        queue = queue.tail
        
        val currentKey = ZoneStateKey(current.inits, current.val_env, current.zone, current.pendingDelays,current.act)
        val sourceId = stateToId(currentKey)
        
        val edgeNexts = RxSemantics.nextEdgeSymbolic(current)
        for ((edge, nextState) <- edgeNexts) {
          val (from, to, tId, label) = edge
          val targetId = getId(nextState)
          
          val nextKey = ZoneStateKey(nextState.inits, nextState.val_env, nextState.zone, nextState.pendingDelays,nextState.act)
          if (!visited.contains(nextKey)) {
            visited += nextKey
            queue = queue :+ nextState
            elements += buildNodeJson(nextState, targetId)
          }
          
          val displayLabel = if (tId == label) label.show else s"${label.show}(${tId.show})"
          elements += s"""{"data": {"id": "e$edgeCounter", "source": "$sourceId", "target": "$targetId", "label": "${sanitizeLabel(displayLabel)}"}}"""
          edgeCounter += 1
        }
      }
       
      s"[${elements.mkString(",")}]"
    }.getOrElse("[]")
  }

  @JSExport
  def findPathToValueZone(condStr: String): js.Any = {
    currentGraph match {
      case Some(rx) =>
        try {
          val formatted = condStr
            .replace("&&", "] &&[")
            .replace("AND", "] && [")
          
          val formulaStr = s"[$formatted]"
          val formula = PdlParser.parsePdlFormula(formulaStr)
          
          def formulaToCondition(f: rta.syntax.Formula): rta.syntax.Condition = f match {
            case rta.syntax.Formula.CondProp(c) => c
            case rta.syntax.Formula.And(f1, f2) => rta.syntax.Condition.And(formulaToCondition(f1), formulaToCondition(f2))
            case _ => throw new Exception("Use apenas comparações simples unidas por && ou AND")
          }

          val finalCond = formulaToCondition(formula)

          val queryConstants = RxSemantics.MaxConstants.fromCond(finalCond, rx.clocks)
          val rxWithBoostedConstants = rx.copy(
            maxConstants = RxSemantics.MaxConstants.mergeMax(rx.maxConstants, queryConstants)
          )

          AnalyseLTS.findShortestPathToCondition(rxWithBoostedConstants, finalCond, maxStates = 50000, useZones = true) match {
            case Some(steps) => js.Array(steps: _*)
            case None => js.Dynamic.literal(error = "Caminho não encontrado ou inatingível (Motor Simbólico / DBM).")
          }
        } catch {
          case e: Throwable => js.Dynamic.literal(error = s"Erro: ${e.getMessage}")
        }
      case None => js.Dynamic.literal(error = "Modelo não carregado.")
    }
  }


  @JSExport
  def debugZoneGraphSize(hardCap: Int = 200000): String = {
    currentGraph match {
      case Some(root) =>
        var visited = Set[ZoneStateKey]()
        var queue = List(root)
        var diagonalViolations = List[String]()

        def diagonalOk(z: rta.backend.DBM.Zone): Boolean =
          z.clocks.forall(c => z.matrix((c, c)).value == 0.0 && !z.matrix((c, c)).strict)

        while (queue.nonEmpty && visited.size <= hardCap) {
          val cur = queue.head; queue = queue.tail
          val key = ZoneStateKey(cur.inits, cur.val_env, cur.zone, cur.pendingDelays, cur.act)
          if (!visited.contains(key)) {
            visited += key
            if (!diagonalOk(cur.zone)) {
              diagonalViolations = s"Zona ${cur.inits.mkString(",")} viola D(x,x)=0" :: diagonalViolations
            }
            queue = queue ++ RxSemantics.nextEdgeSymbolic(cur).map(_._2)
          }
        }

        val exhaustive = visited.size <= hardCap
        s"""{"zoneCount": ${visited.size}, "exhaustive": $exhaustive, "diagonalViolations": ${diagonalViolations.size}, "sampleViolations": ${js.JSON.stringify(js.Array(diagonalViolations.take(5): _*))}}"""
      case None => """{"error": "Modelo não carregado."}"""
    }
  }


  @JSExport
  def verifyLTLBatch(formulaStr: String, traceLength: Int, batchSize: Int): String = {
    currentGraph match {
      case Some(startGraph) =>
        try {
          val formula = LtlParser.parseLtlFormula(formulaStr)
          var allTrue = true
          var counterExampleTrace: List[String] = Nil
          var counterExampleIds: List[String] = Nil
          var passedCount = 0
          var sampleTrace: List[String] = Nil
          var sampleIds: List[String] = Nil

          for (_ <- 1 to batchSize if allTrue) {
            val (trace, pathLabels, edgeIds) = TraceGenerator.randomTimedTraceDetailed(startGraph, traceLength)
            
            val res = LtlEvaluator.eval(trace, formula, LtlEvaluator.Hybrid) 
            if (!res) {
              allTrue = false
              counterExampleTrace = pathLabels
              counterExampleIds = edgeIds
            } else {
              passedCount += 1
              sampleTrace = pathLabels 
              sampleIds = edgeIds
            }
          }

          val sampleStr = if (sampleTrace.isEmpty) "Traço Vazio/Deadlock" else sampleTrace.mkString(" ➔ ")
          val ceStr = if (counterExampleTrace.isEmpty) "Traço Vazio/Deadlock" else counterExampleTrace.mkString(" ➔ ")
          val pathJson = js.JSON.stringify(js.Array(counterExampleIds: _*))
          val samplePathJson = js.JSON.stringify(js.Array(sampleIds: _*))

          if (allTrue) {
            s"""{"success": true, "passedCount": $passedCount, "sample": "${escapeJson(sampleStr)}", "samplePath": $samplePathJson}"""
          } else {
            s"""{"success": false, "passedCount": $passedCount, "sample": "${escapeJson(sampleStr)}", "samplePath": $samplePathJson, "counterExample": "${escapeJson(ceStr)}", "path": $pathJson}"""
          }
        } catch {
          case e: Throwable => s"""{"error": "${escapeJson(s"Erro LTL: ${e.getMessage}")}"}"""
        }
      case None => """{"error": "Modelo não carregado."}"""
    }
  }

  @JSExport
  def testLTLEquivalence(retaFormulaStr: String, regaFormulaStr: String, traceLength: Int): String = {
    currentGraph match {
      case Some(rx) =>
        try {
          val retaFormula = LtlParser.parseLtlFormula(retaFormulaStr)
          val regaFormula = LtlParser.parseLtlFormula(regaFormulaStr)

          val gltsSource = RTATranslator.translate_syntax(rx, currentSource)
          val gltsGraph = Parser2.parseProgram(gltsSource)

          val (retaTrace, pathLabels) = TraceGenerator.randomTrace(rx, traceLength)
          val regaTrace = TraceGenerator.followPath(gltsGraph, pathLabels)

          val resReta = LtlEvaluator.eval(retaTrace, retaFormula, LtlEvaluator.ReTA)
          val resRega = LtlEvaluator.eval(regaTrace, regaFormula, LtlEvaluator.ReGA)

          val traceStr = pathLabels.mkString(" ➔ ")
          s"""
          | === TEOREMA 1: PROVA DE EQUIVALÊNCIA REAL ===
          |
          | Traço percorrido (σ) [${pathLabels.size} passos]:
          | Start ➔ $traceStr
          |
          | [ReTA Semantics] σ ⊨ φ : $resReta
          | [ReGA (GLTS) Semantics] τ ⊨ ∆φ : $resRega
          |
          | Equivalência: ${if (resReta == resRega) "VERIFICADO ✓" else "FALHOU ✗"}
          """.stripMargin
        } catch {
          case e: Throwable => s"Erro ao testar equivalência LTL:\n${e.getMessage}"
        }
      case None => "Erro: Carrega um modelo primeiro."
    }
  }

  @JSExport
  def getExamples(): String = {
    val examples = List(

  "Conditions" ->
    """name Conditions
      |int counter = 0
      |init start
      |start ---> middle: step1  if (counter < 2) then {
      |  counter' := counter + 1
      |}
      |middle ---> endN: activateStep2 if (counter == 1)""".stripMargin,
  "LikeAlgorithm" ->
      """name LikeAlgorithm
      |init Feed
      |Feed ---> Watch: watch
      |Watch ---> Watch: like
      |Watch ---> Feed: dontLike
      |Watch ---> Feed: refresh disabled
      |Feed ---> List: watchLike disabled
      |List ---> Watch: watch2
      |watch ->> dontLike: wd
      |like --! dontLike: ld
      |like ->> refresh: lr
      |like ->> watchLike: lw
      |dontLike --! watchLike: dw""".stripMargin,
  "likeRTA" ->
  """name Like2
  |clock x;
  |inv FEED:x<=20
  |inv WATCH:x<=120
  |inv LIST:x<=20
  |init FEED
  |
  |FEED -watch-> WATCH: fww if (x>=1) then {
  |  x':=0
  |}
  |
  |FEED -seeList-> LIST: fsl disabled if (x>=1) then {
  |  x':=0
  |}
  |
  |WATCH -like-> FEED: wlf disabled if (x>=10) then {
  |  x':=0
  |}
  |
  |WATCH -dontLike-> FEED: wdf if (x>=1 AND x<=9) then {
  |  x':=0
  |}
  |
  |LIST -wacth-> WATCH: lww if (x>=1) then {
  |  x':=0
  |}
  |
  |fww ->> wlf:onLike
  |wdf --! wlf:offLike
  |wlf ->> fsl:onSee
  |wdf --! fsl:offSee
  """.stripMargin,

  "GRG" ->
   """name GRG
      |int a_active   = 1
      |int b_active   = 0
      |int c_active = 0
      |
      |init s0
      |
      |s0 ---> s1: aa  if (a_active == 1) then {
      |  b_active' := 1;
      |  if (c_active == 1) then {
      |  	a_active' := 0
      |  }
      |}
      |
      |s1 ---> s0: bb  if (b_active == 1) then {
      |  c_active' := 1;
      |  if (a_active == 0) then {
      |  	b_active' := 0
      |  }
      |}
      |
      |s1 ---> s2: cc  if (c_active == 1)
      |
      |
      |aa --! aa: offA2 disabled
      |aa ->> bb: onB if (b_active == 0)
      |bb ->> offA2: onOffA if (c_active == 0)
      |""".stripMargin,
  "TIMER" ->
  """name TIMER
    |clock t;
    |init s0;
    |inv s1: t <= 10;
    |int c = 0
    |s0 ---> s1: start if(c==0) then {
    |  t' := 0;
    |}
    |
    |
    |s1 ---> s2: timeout if (t >= 10)
    |
    |s1 ---> s0: escape if (t < 5)
    |""".stripMargin,

  "Counter" ->
    """name Counter
      |init s0
      |s0 ---> s0: act
      |act --! act: offAct disabled
      |act ->> offAct: on1 disabled
      |act ->> on1""".stripMargin,



  "Vending (max eur1)" ->
    """name Vending1eur
      |init Insert
      |Insert ---> Coffee: ct50
      |Insert ---> Chocolate: eur1
      |Coffee ---> Insert: GetCoffee
      |Chocolate ---> Insert: GetChoc
      |
      |eur1 --! ct50
      |eur1 --! eur1
      |ct50 --! ct50: lastct50 disabled
      |ct50 --! eur1
      |ct50 ->> lastct50""".stripMargin,

  "Vending (max 3prod)" ->
    """name Vending3prod
      |init pay
      |pay ---> select: insertCoin
      |select ---> soda: askSoda
      |select ---> beer: askBeer
      |soda ---> pay: getSoda
      |beer ---> pay: getBeer
      |
      |askSoda --! askSoda: noSoda disabled
      |askBeer --! askBeer: noBeer
      |askSoda ->> noSoda""".stripMargin,

  "Intrusive product" ->
    """name IntrusiveProduct
      |aut s {
      |  init i0
      |  i0 ---> i1: a
      |  i1 ---> i2: b
      |  i2 ---> i0: d disabled
      |  a --! b
      |}
      |aut w {
      |  init i0
      |  i0 ---> i1: a
      |  i1 ---> i0: c
      |  a --! a: noAs disabled
      |  a ->> noAs
      |}
      |// intrusion
      |w.c ->> s.b""".stripMargin,
    )
    "{" + examples.map{ case (k,v) => s""""$k": ${js.JSON.stringify(v)}""" }.mkString(",") + "}"
  }

  @JSExport
  def getCurrentStateText(): String = currentGraph.map(_.toString).getOrElse("")

  @JSExport
  def getCurrentStateMermaid(): String = currentGraph.map(g => RxGraph.toMermaid(g)).getOrElse("")

  @JSExport
  def getCurrentStateMermaidSimple(): String = currentGraph.map(g => RxGraph.toMermaidPlain(g)).getOrElse("")




  @JSExport
  def getSymbolicStepsMermaid(): String = {
    import rta.backend.AnalyseLTS.ZoneStateKey

    def sanitizeForMermaid(s: String): String = {
      s.replace("<=", "≤")
       .replace(">=", "≥")
       .replace("<", "⋖")
       .replace(">", "⋗")
    }

    currentGraph.map { root =>
      var visited = Set[ZoneStateKey]()
      var queue = List(root)
      var transitionsStr = List[String]()
      
      var stateToId = Map[ZoneStateKey, Int]()
      var idCounter = 0
      
      def getId(g: RxGraph): Int = {
        val key = ZoneStateKey(g.inits, g.val_env, g.zone, g.pendingDelays,g.act)
        stateToId.getOrElse(key, {
          idCounter += 1
          stateToId += (key -> idCounter)
          idCounter
        })
      }
      
      val startKey = ZoneStateKey(root.inits, root.val_env, root.zone, root.pendingDelays,root.act)
      stateToId += (startKey -> 0)

      val maxStates = 80 

      while(queue.nonEmpty && visited.size < maxStates) {
        val current = queue.head
        queue = queue.tail
        
        val currentKey = ZoneStateKey(current.inits, current.val_env, current.zone, current.pendingDelays,current.act)
        if (!visited.contains(currentKey)) {
          visited += currentKey
          val sourceId = getId(current)
          
          val edgeNexts = RxSemantics.nextEdgeSymbolic(current)
          for ((edge, nextState) <- edgeNexts) {
            val (from, to, tId, label) = edge
            val targetId = getId(nextState)
            
            val displayLabel = if (tId == label) label.show else s"${label.show}(${tId.show})"
            transitionsStr = s"""$sourceId --->|"$displayLabel"| $targetId""" :: transitionsStr
            
            val nextKey = ZoneStateKey(nextState.inits, nextState.val_env, nextState.zone, nextState.pendingDelays,nextState.act)
            if (!visited.contains(nextKey)) {
              queue = queue :+ nextState
            }
          }
        }
      }
       
      val nodeDefinitions = stateToId.map { case (key, id) =>
        val zoneStr = sanitizeForMermaid(showZoneCompact(key.zone))
        val varsStr = sanitizeForMermaid(key.vars.filterNot(_._1.n.contains("__return")).map(kv => s"${kv._1.show}=${kv._2.value}").mkString(", "))
        val pendingStr = sanitizeForMermaid(key.pending.map { case (edge, op, clock, target) =>
          s"${edge._4.show}(${op})@${clock.show}=${target}"
        }.mkString(", "))
        
        val labelParts = List(
          key.inits.mkString(", "),
          if (zoneStr.nonEmpty) s"Zone: $zoneStr" else "Zone: t ≥ 0",
          if (varsStr.nonEmpty) s"Vars: $varsStr" else "",
          if (pendingStr.nonEmpty) s"Pending: $pendingStr" else ""
        ).filter(_.nonEmpty)
        
        val label = labelParts.mkString("\n")
        val style = if (id == 0) {
          s"\nstyle $id fill:#c7d2fe,stroke:#4f46e5,stroke-width:2px,font-size:32px,font-weight:bold"
        } else {
          s"\nstyle $id font-size:32px,font-weight:bold"
        }
        s"""$id["$label"]$style"""
      }.mkString("\n")

      s"""graph TD
         |${transitionsStr.distinct.reverse.mkString("\n")}
         |$nodeDefinitions
         |""".stripMargin
      
    }.getOrElse("graph LR\n0(Nenhum modelo carregado)")
  }

  private def showZoneCompact(z: rta.backend.DBM.Zone): String = {
    import rta.backend.DBM._
    val list = for {
      x <- z.clocks if x != ZERO_CLOCK
    } yield {
      val upperOpt = z.matrix.get((x, ZERO_CLOCK))
      val lowerOpt = z.matrix.get((ZERO_CLOCK, x))
      
      val uStr = upperOpt match {
        case Some(Bound(v, strict)) if v != Double.PositiveInfinity =>
          s"${if (strict) "<" else "<="} $v"
        case _ => ""
      }
      val lStr = lowerOpt match {
        case Some(Bound(v, strict)) if v != Double.PositiveInfinity =>
          s"${if (strict) ">" else ">="} ${-v}"
        case _ => ""
      }
      
      if (lStr.nonEmpty && uStr.nonEmpty) {
        val lVal = -lowerOpt.get.value
        val uVal = upperOpt.get.value
        val lOp = if (lowerOpt.get.strict) "<" else "<="
        val uOp = if (upperOpt.get.strict) "<" else "<="
        s"$lVal $lOp ${x.show} $uOp $uVal"
      } else if (lStr.nonEmpty) {
        s"${x.show} $lStr"
      } else if (uStr.nonEmpty) {
        s"${x.show} $uStr"
      } else {
        s"${x.show} >= 0"
      }
    }
    list.filter(_.nonEmpty).mkString(", ")
  }

  private def stringToQName(str: String): QName = if (str.isEmpty) QName(Nil) else QName(str.split('/').toList)
  
  private def generateSimulationJson(graph: RxGraph, traversedEdge: Option[Edge]): String = {
     val graphElementsJson = CytoscapeConverter(graph)
     
     val eventTransitions = RxSemantics.nextEdge(graph).map(_._1)
     val eventTransitionsJson = eventTransitions.map { case (from, to, tId, label) =>
       val displayName = if (tId == label) label.show else s"${label.show}(${tId.show})"
       s"""{"from":"$from", "to":"$to", "tId":"$tId", "label":"$label", "displayName":"$displayName", "isDelay": false}"""
     }.mkString(",")

     val delayTransitionJson = if (RxSemantics.nextDelay(graph).nonEmpty) s"""{"label":"delay", "displayName":"⏱ delay", "isDelay": true}""" else ""
     val allEnabledTransitions = Seq(eventTransitionsJson, delayTransitionJson).filter(_.nonEmpty).mkString(",")

     val clocksJson = graph.clock_env.map { case (n, v) => s""""${n.show}": $v""" }.mkString(",")
     val valEnvJson = graph.val_env.map { case (n, v) => s""""${n.show}": ${runtimeValueToJson(v)}""" }.mkString(",")

     val pendingJson = graph.pendingDelays.map { case (edge, op, clk, target) =>
       val lbl = edge._4.show
       s"""{"label": "$lbl", "op": "$op", "clock": "${clk.show}", "target": $target}"""
     }.mkString("[", ",", "]")

     val traversedJson = traversedEdge match {
       case Some((from, to, tId, label)) => s"""{"from":"$from", "to":"$to", "tId":"$tId", "label":"$label"}"""
       case None => "null"
     }

     s"""
       |{
       |  "graphElements": $graphElementsJson,
       |  "panelData": { 
       |     "enabled": [$allEnabledTransitions], 
       |     "clocks": {$clocksJson}, 
       |     "variables": {$valEnvJson}, 
       |     "pending": $pendingJson,
       |     "canUndo": ${history.size > 1} 
       |  },
       |  "lastTransition": $traversedJson
       |}
       |""".stripMargin
  }
}