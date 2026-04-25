package rta.frontend

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import rta.syntax.Parser2
import rta.syntax.Program2.{RxGraph, Edge, QName}
import rta.backend.{RxSemantics, CytoscapeConverter, PdlEvaluator, MCRL2, UppaalConverter3, AnalyseLTS}
import rta.syntax.PdlParser
import rta.syntax.RTATranslator
import rta.syntax.Condition

@JSExportTopLevel("RTA")
object RTAAPI {

  private var currentGraph: Option[RxGraph] = None
  private var currentSource: String = ""
  private var history: List[RxGraph] = Nil

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
          val (from, to, tId, label) = edge // Desestruturação da quádrupla
          val targetId = getId(nextState)
          
          // Mostra Label(ID) se forem diferentes
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
              case Some(inv) => Condition.evaluate(inv, nextTimeState.val_env, nextTimeState.clock_env)
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
      val graph = Parser2.parseProgram(sourceCode)
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
          
          // O JS agora deve enviar tanto o tId quanto o label
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

  @JSExport
  def advanceTime(delayAmount: Double): String = {
    currentGraph match {
      case Some(currentState) =>
        if (currentState.clocks.isEmpty || delayAmount <= 0) {
          generateSimulationJson(currentState, None)
        } else {
          val delayedClockEnv = currentState.clock_env.map { case (c, v) => (c, v + delayAmount) }
          val potentialNextState = currentState.copy(clock_env = delayedClockEnv)

          val allInvariantsHold = potentialNextState.inits.forall { s =>
            potentialNextState.invariants.get(s) match {
              case Some(inv) => Condition.evaluate(inv, potentialNextState.val_env, potentialNextState.clock_env)
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

  //@JSExport
  //def getUppaalGLTS(): String = currentGraph.map(g => UppaalConverter2.convert(g, currentSource)).getOrElse("")
  
  //@JSExport
  //def getUppaalRG(): String = currentGraph.map(g => UppaalConverter.convert(g, currentSource)).getOrElse("")
  
  @JSExport
  def getUppaalTGRG(layoutJson: String): String = currentGraph.map(g => UppaalConverter3.convert(g, currentSource, layoutJson)).getOrElse("")

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
  def runPdl(stateStr: String, formulaStr: String): String = {
    currentGraph match {
      case Some(rx) =>
        try {
          val adaptedState = stateStr.replace('/', '.')
          Parser2.pp[QName](Parser2.qname, adaptedState) match {
            case Left(err) => s"Error parsing state '$stateStr': $err"
            case Right(startState) =>
              if (!rx.states.contains(startState)) {
                 s"State '${startState.show}' not found in the current model."
              } else {
                 val formula = PdlParser.parsePdlFormula(formulaStr)
                 val result = PdlEvaluator.evaluateFormula(startState, formula, rx)
                 s"Result: $result"
              }
          }
        } catch {
          case e: Throwable => 
            val msg = if (e.getMessage != null) e.getMessage else e.toString
            s"Evaluation Error: $msg"
        }
      case None => "Model not loaded."
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


  private def stringToQName(str: String): QName = if (str.isEmpty) QName(Nil) else QName(str.split('/').toList)
  
  private def generateSimulationJson(graph: RxGraph, traversedEdge: Option[Edge]): String = {
     val graphElementsJson = CytoscapeConverter(graph)
     
     val eventTransitions = RxSemantics.nextEdge(graph).map(_._1)
     val eventTransitionsJson = eventTransitions.map { case (from, to, tId, label) =>
       // displayName formatado como Label(transID) se forem diferentes
       val displayName = if (tId == label) label.show else s"${label.show}(${tId.show})"
       s"""{"from":"$from", "to":"$to", "tId":"$tId", "label":"$label", "displayName":"$displayName", "isDelay": false}"""
     }.mkString(",")

     val delayTransitionJson = if (RxSemantics.nextDelay(graph).nonEmpty) s"""{"label":"delay", "displayName":"⏱ delay", "isDelay": true}""" else ""
     val allEnabledTransitions = Seq(eventTransitionsJson, delayTransitionJson).filter(_.nonEmpty).mkString(",")

     val clocksJson = graph.clock_env.map { case (n, v) => s""""${n.show}": $v""" }.mkString(",")
     val valEnvJson = graph.val_env.map { case (n, v) => s""""${n.show}": $v""" }.mkString(",")

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
       |     "canUndo": ${history.size > 1} 
       |  },
       |  "lastTransition": $traversedJson
       |}
       |""".stripMargin
  }
}