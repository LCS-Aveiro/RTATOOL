package rta.backend

import rta.syntax.Program2.RxGraph
import scala.util.Random
import scala.annotation.tailrec

object TraceGenerator {

  def randomTrace(start: RxGraph, length: Int): (List[RxGraph], List[String]) = {
    var trace = List(start)
    var labels = List[String]()
    var current = start
    val rand = new Random()

    for (_ <- 0 until length) {
      val nexts = RxSemantics.nextEdge(current).toList
      if (nexts.nonEmpty) {
        val step = nexts(rand.nextInt(nexts.size))
        current = step._2
        trace = trace :+ current
        labels = labels :+ step._1._4.show
      }
    }
    (trace, labels)
  }

  def randomTraceDetailed(start: RxGraph, length: Int): (List[RxGraph], List[String], List[String]) = {
    var trace = List(start)
    var labels = List[String]()
    var edgeIds = List[String]()
    var current = start
    val rand = new Random()

    for (_ <- 0 until length) {
      val nexts = RxSemantics.nextEdge(current).toList
      if (nexts.nonEmpty) {
        val step = nexts(rand.nextInt(nexts.size))
        current = step._2
        trace = trace :+ current
        val edge = step._1
        labels = labels :+ edge._4.show
        edgeIds = edgeIds :+ s"event_${edge._1}_${edge._2}_${edge._3}_${edge._4}"
      }
    }
    (trace, labels, edgeIds)
  }

  def followPath(start: RxGraph, labels: List[String]): List[RxGraph] = {
    @tailrec
    def loop(current: RxGraph, remaining: List[String], acc: List[RxGraph]): List[RxGraph] = remaining match {
      case Nil => acc
      case lbl :: tail =>
        RxSemantics.nextEdge(current).toList.find(_._1._4.show == lbl) match {
          case Some(step) => loop(step._2, tail, acc :+ step._2)
          case None => acc
        }
    }
    loop(start, labels, List(start))
  }


  def randomTimedTraceDetailed(start: RxGraph, length: Int): (List[RxGraph], List[String], List[String]) = {
    var trace = List(start)
    var labels = List[String]()
    var edgeIds = List[String]()
    var current = start
    val rand = new scala.util.Random()

    for (_ <- 0 until length) {
      val nextEdges = RxSemantics.nextEdge(current).toList
      val canDelay = current.clocks.nonEmpty

      if (nextEdges.isEmpty && !canDelay) {
        // Deadlock total (nem arestas, nem tempo)
      } else {
        // Decide aleatoriamente se avança o tempo ou dispara uma aresta
        val doDelay = canDelay && (nextEdges.isEmpty || rand.nextBoolean())
        
        if (doDelay) {
          // Calcula quanto tempo falta para o próximo hiper-evento pendente disparar
          val pendingDiffs = current.pendingDelays.map { case (_, _, clock, targetVal) =>
            targetVal - current.clock_env.getOrElse(clock, 0.0)
          }.filter(_ > 0.00001)
          
          // Se houver timeouts pendentes, salta o tempo exato para o disparar. 
          // Se não houver, salta um valor aleatório (ex: até 3.0 segundos)
          val delayVal = if (pendingDiffs.nonEmpty) pendingDiffs.min else (0.1 + rand.nextDouble() * 3.0)
          
          RxSemantics.advanceTimeBy(current, delayVal) match {
            case Some(nextState) =>
              current = nextState
              trace = trace :+ current
              labels = labels :+ f"Delay($delayVal%.2fs)"
              edgeIds = edgeIds :+ "delay_node" // ID fantasma (ignorado na animação)
            case None =>
              // Se não conseguir avançar no tempo (ex: violação de inv), dispara uma aresta se possível
              if (nextEdges.nonEmpty) {
                val step = nextEdges(rand.nextInt(nextEdges.size))
                current = step._2
                trace = trace :+ current
                labels = labels :+ step._1._4.show
                edgeIds = edgeIds :+ s"event_${step._1._1}_${step._1._2}_${step._1._3}_${step._1._4}"
              }
          }
        } else if (nextEdges.nonEmpty) {
          // Dispara instantaneamente uma transição normal
          val step = nextEdges(rand.nextInt(nextEdges.size))
          current = step._2
          trace = trace :+ current
          labels = labels :+ step._1._4.show
          edgeIds = edgeIds :+ s"event_${step._1._1}_${step._1._2}_${step._1._3}_${step._1._4}"
        }
      }
    }
    (trace, labels, edgeIds)
  }


}