package rta

import com.sun.net.httpserver.{HttpServer, HttpHandler, HttpExchange}
import java.net.InetSocketAddress
import java.awt.Desktop
import java.net.URI
import java.io.{File, PrintWriter}
import scala.io.Source

import rta.syntax.{Parser2, PdlParser, LtlParser, Program2, RTATranslator}
import rta.syntax.Program2.{RxGraph, QName}
import rta.backend.{RxSemantics, UppaalConverter3, PdlEvaluator, AnalyseLTS}

object RTACLI {

  def main(args: Array[String]): Unit = {
    if (args.isEmpty || args(0) == "-server") {
      runServerMode()
    } else {
      runCliMode(args)
    }
  }

  def runCliMode(args: Array[String]): Unit = {
    val command = args(0)
    val inputFile = args.last
    
    if (command == "-help" || args.length < 2) {
      printHelp()
      return
    }

    try {
      val source = Source.fromFile(inputFile).mkString
      
      // 1. Parsing do Código
      val parsedGraph = Parser2.parseProgram(source)
      
      // 2. Cálculo das Constantes Máximas para garantir Finitude do Espaço (Zonas DBM)
      val maxC = RxSemantics.MaxConstants.compute(parsedGraph)
      val graph = parsedGraph.copy(maxConstants = maxC)

      command match {
        case "-text" =>
          println(graph.toString)

        case "-mermaid" =>
          println(RxGraph.toMermaid(graph))

        case "-translate" =>
          val translation = RTATranslator.translate_syntax(graph, source)
          if (args.length > 2) {
            val outName = args(1)
            if (outName != inputFile) {
               new PrintWriter(outName) { write(translation); close() }
               println(s"Tradução salva em $outName")
            } else println(translation)
          } else {
            println(translation)
          }
        

          
        case "-uppaal" =>
          val xml = UppaalConverter3.convert(graph, source) // SEM O "{}" AQUI
          saveOrPrint(xml, args, inputFile)

        case "-step" =>
          val transitions = RxSemantics.nextEdge(graph)
          if (transitions.isEmpty) println("Deadlock: Nenhuma transição habilitada.")
          else {
            println(s"Estado Atual: ${graph.inits.mkString(", ")}")
            println("Transições Habilitadas:")
            transitions.foreach { case ((from, to, tId, lbl), _) =>
              val display = if (tId == lbl) lbl.show else s"${lbl.show}(${tId.show})"
              println(s"  - [$display] de ${from.show} para ${to.show}")
            }
            val delays = RxSemantics.nextDelay(graph)
            if (delays.nonEmpty) println("  - [delay] Passagem de tempo permitida")
          }

        case "-lts" =>
          println(generateLTSMermaid(graph))

        case "-ltl" =>
          if (args.length < 3) {
            println("Uso: -ltl <formula_ltl> <arquivo>")
          } else {
            val formulaStr = args(1)
            try {
              val formula = LtlParser.parseLtlFormula(formulaStr)
              
              // Booster de constantes para assegurar corretude com a query LTL
              val queryConstants = RxSemantics.MaxConstants.fromLTL(formula, graph.clocks)
              val boostedStartGraph = graph.copy(
                maxConstants = RxSemantics.MaxConstants.mergeMax(graph.maxConstants, queryConstants)
              )

              // CUIDADO: O 5º elemento retornado é 'limitReached' (se atingiu o limite ou não)
              val (success, ceLabels, _, explored, limitReached, _) = 
                  AnalyseLTS.verifyLTLSymbolic(boostedStartGraph, formula, 50000, 100)
              
              val isExhaustive = !limitReached
              
              // --- OUTPUT FORMATADO PARA O SCRIPT PYTHON ---
              println("--- RESULTADOS LTL ---")
              println(s"[RESULT] ${if (success) "VALID" else "FAILED"}")
              println(s"[EXHAUSTIVE] ${if (isExhaustive) "TRUE" else "FALSE"}")
              println(s"[EXPLORED] $explored")
              if (!success) {
                println(s"[TRACE] Start -> ${ceLabels.mkString(" -> ")}")
              }
              println("----------------------")

            } catch {
              case e: Exception if e.getMessage != null && e.getMessage.contains("zone-splitting") =>
                println("--- RESULTADOS LTL ---")
                println("[RESULT] ERROR_CLOCK_LTL")
                println("----------------------")
              case e: Exception =>
                println("--- RESULTADOS LTL ---")
                println(s"[RESULT] ERROR_PARSING")
                println("----------------------")
            }
          }

        case "-pdl" =>
          if (args.length < 4) {
            println("Uso: -pdl <estado_inicial> <formula_pdl> <arquivo>")
          } else {
            val stateStr = args(1)
            val formulaStr = args(2)
            
            val qnameRes = try { 
               Right(QName(stateStr.split('.').toList))
            } catch { case e: Exception => Left(e.getMessage) }

            qnameRes match {
              case Right(startState) =>
                if (!graph.states.contains(startState)) {
                  println(s"Erro: Estado '$stateStr' não encontrado no modelo.")
                } else {
                  val formula = PdlParser.parsePdlFormula(formulaStr)
                  val result = PdlEvaluator.evaluateFormula(startState, formula, graph)
                  println(s"Modelo: $inputFile")
                  println(s"Estado: $stateStr")
                  println(s"Fórmula: $formulaStr")
                  println(s"Resultado: $result")
                }
              case Left(err) => println(s"Erro ao ler estado: $err")
            }
          }

        case _ => printHelp()
      }

    } catch {
      case e: java.io.FileNotFoundException => println(s"Arquivo não encontrado: $inputFile")
      case e: Exception => 
        println("Erro durante a execução:")
        e.printStackTrace()
    }
  }

  // Auxiliar para os comandos Uppaal e Translate
  private def saveOrPrint(content: String, args: Array[String], inputFile: String): Unit = {
    if (args.length > 2) {
      val outName = args(1)
      if (outName != inputFile) {
         new PrintWriter(outName) { write(content); close() }
         println(s"Ficheiro gerado com sucesso em $outName")
      } else println(content)
    } else {
      println(content)
    }
  }

  def generateLTSMermaid(root: RxGraph): String = {
    var visited = Set[RxGraph](root)
    var queue = List(root)
    var transitionsStr = List[String]()
    
    var stateToId = Map[RxGraph, Int](root -> 0)
    var idCounter = 0
    
    def getId(g: RxGraph): Int = {
      if (stateToId.contains(g)) stateToId(g)
      else {
        idCounter += 1
        stateToId += (g -> idCounter)
        idCounter
      }
    }

    val maxStates = 2000

    while(queue.nonEmpty && visited.size < maxStates) {
      val current = queue.head
      queue = queue.tail
      val sourceId = getId(current)
      
      val nexts = RxSemantics.nextEdge(current)
      
      for ((edge, nextState) <- nexts) {
        val targetId = getId(nextState)
        val label = edge._4.show // Nova Quadra Edge -> Label está em _4
        transitionsStr = s"$sourceId -->|\"$label\"| $targetId" :: transitionsStr
        
        if (!visited.contains(nextState)) {
          visited += nextState
          queue = queue :+ nextState
        }
      }
    }

    val nodes = stateToId.map { case (st, id) =>
      val lbl = st.inits.mkString(",")
      val style = if(st == root) "style " + id + " fill:#bbf,stroke:#333,stroke-width:2px" else ""
      s"$id(\"$lbl\")\n$style"
    }.mkString("\n")

    s"""graph LR
       |${transitionsStr.reverse.mkString("\n")}
       |$nodes
       |""".stripMargin
  }

  def printHelp(): Unit = {
    println(
      """
        |Uso: java -jar RTATool.jar [COMANDO] [OPCOES] <ARQUIVO>
        |
        |Sem argumentos: Abre a Interface Grafica (Navegador local).
        |
        |Comandos CLI:
        |  -translate <arquivo>         : Traduz o codigo para GLTS (stdout)
        |  -translate <saida> <arq>     : Traduz e salva em arquivo
        |  -uppaal <arquivo>            : Exporta XML Uppaal (V4 com Delays, recomendado)
        |  -uppaalRG <arquivo>          : Exporta XML Uppaal (RG Legado)
        |  -text <arquivo>              : Imprime representacao textual do estado inicial
        |  -mermaid <arquivo>           : Imprime representacao Mermaid do estado inicial
        |  -step <arquivo>              : Lista as transicoes habilitadas (Run Step 1)
        |  -lts <arquivo>               : Gera o diagrama Mermaid de TODOS os passos do LTS Discreto
        |  -ltl <form> <arq>            : Verifica form. LTL usando DBM Exaustivo (Ex: -ltl "F s2" mdl.txt)
        |  -pdl <estado> <form> <arq>   : Avalia formula PDL (Ex: -pdl "s0" "[a]true" mdl.txt)
        |""".stripMargin)
  }

  class ResourceHandler extends HttpHandler {
    override def handle(t: HttpExchange): Unit = {
      var path = t.getRequestURI.getPath
      if (path == "/" || path == "") path = "/index.html"
      val stream = getClass.getResourceAsStream(path)
      if (stream == null) {
        t.sendResponseHeaders(404, 0); t.getResponseBody.close()
      } else {
        if (path.endsWith(".html")) t.getResponseHeaders.set("Content-Type", "text/html; charset=utf-8")
        else if (path.endsWith(".js")) t.getResponseHeaders.set("Content-Type", "application/javascript")
        else if (path.endsWith(".css")) t.getResponseHeaders.set("Content-Type", "text/css")
        t.sendResponseHeaders(200, 0)
        stream.transferTo(t.getResponseBody)
        stream.close(); t.getResponseBody.close()
      }
    }
  }

  def runServerMode(): Unit = {
    val server = HttpServer.create(new InetSocketAddress("localhost", 0), 0)
    server.createContext("/", new ResourceHandler())
    server.start()
    val url = s"http://localhost:${server.getAddress.getPort}/index.html"
    println(s"Interface Grafica RTA disponivel em: $url")
    
    if (Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE)) {
      Desktop.getDesktop.browse(new URI(url))
    }
    Thread.currentThread().join()
  }
}