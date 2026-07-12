package rta.backend

import rta.syntax.Program2.RxGraph
import scala.util.Random
import scala.annotation.tailrec
import scala.util.boundary, boundary.break

object TraceGenerator {

  def randomTrace(start: RxGraph, length: Int): (List[RxGraph], List[String]) = {
    val trace = collection.mutable.ListBuffer(start)
    val labels = collection.mutable.ListBuffer[String]()
    var current = start
    val rand = new Random()

    boundary {
      for (_ <- 0 until length) {
        val nexts = RxSemantics.nextEdge(current).toList
        if (nexts.nonEmpty) {
          val step = nexts(rand.nextInt(nexts.size))
          current = step._2
          trace += current
          labels += step._1._4.show
        } else {
          break()
        }
      }
    }
    (trace.toList, labels.toList)
  }

  def randomTraceDetailed(start: RxGraph, length: Int): (List[RxGraph], List[String], List[String]) = {
    val trace = collection.mutable.ListBuffer(start)
    val labels = collection.mutable.ListBuffer[String]()
    val edgeIds = collection.mutable.ListBuffer[String]()
    var current = start
    val rand = new Random()

    boundary {
      for (_ <- 0 until length) {
        val nexts = RxSemantics.nextEdge(current).toList
        if (nexts.nonEmpty) {
          val step = nexts(rand.nextInt(nexts.size))
          current = step._2
          trace += current
          val edge = step._1
          labels += edge._4.show
          edgeIds += s"event_${edge._1}_${edge._2}_${edge._3}_${edge._4}"
        } else {
          break()
        }
      }
    }
    (trace.toList, labels.toList, edgeIds.toList)
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
    val trace = collection.mutable.ListBuffer(start)
    val labels = collection.mutable.ListBuffer[String]()
    val edgeIds = collection.mutable.ListBuffer[String]()
    var current = start
    val rand = new scala.util.Random()

    boundary {
      for (_ <- 0 until length) {
        val nextEdges = RxSemantics.nextEdge(current).toList
        val canDelay = current.clocks.nonEmpty

        if (nextEdges.isEmpty && !canDelay) {
          break()
        } else if (!canDelay) {
          val step = nextEdges(rand.nextInt(nextEdges.size))
          current = step._2
          trace += current
          labels += step._1._4.show
          edgeIds += s"event_${step._1._1}_${step._1._2}_${step._1._3}_${step._1._4}"
        } else {
          def findMaxDelay(rx: RxGraph, limit: Double = 100.0): Double = {
            if (RxSemantics.advanceTimeBy(rx, limit).isDefined) return limit
            var low = 0.0
            var high = limit
            for (_ <- 1 to 50) {
              val mid = (low + high) / 2
              if (RxSemantics.advanceTimeBy(rx, mid).isDefined) low = mid
              else high = mid
            }
            math.max(0.0, low - 0.000002) 
          }

          val maxDelay = findMaxDelay(current)
          
          if (maxDelay < 0.001) {
            if (nextEdges.nonEmpty) {
              val step = nextEdges(rand.nextInt(nextEdges.size))
              current = step._2
              trace += current
              labels += step._1._4.show
              edgeIds += s"event_${step._1._1}_${step._1._2}_${step._1._3}_${step._1._4}"
            } else {
              break()
            }
          } else {
            val doDelay = nextEdges.isEmpty || rand.nextBoolean()
            
            if (doDelay) {
              val pendingDiffs = current.pendingDelays.map { case (_, _, clock, targetVal) =>
                targetVal - current.clock_env.getOrElse(clock, 0.0)
              }.filter(_ > 0.00001)

              val baseDelay = if (pendingDiffs.nonEmpty) pendingDiffs.min else (0.1 + rand.nextDouble() * 3.0)
              
              val delayVal = math.min(baseDelay, maxDelay)
              
              RxSemantics.advanceTimeBy(current, delayVal) match {
                case Some(nextState) =>
                  current = nextState
                  trace += current
                  labels += f"Delay($delayVal%.2fs)"
                  edgeIds += "delay_node"
                case None =>
                  if (nextEdges.nonEmpty) {
                    val step = nextEdges(rand.nextInt(nextEdges.size))
                    current = step._2
                    trace += current
                    labels += step._1._4.show
                    edgeIds += s"event_${step._1._1}_${step._1._2}_${step._1._3}_${step._1._4}"
                  } else {
                    break()
                  }
              }
            } else {
              val step = nextEdges(rand.nextInt(nextEdges.size))
              current = step._2
              trace += current
              labels += step._1._4.show
              edgeIds += s"event_${step._1._1}_${step._1._2}_${step._1._3}_${step._1._4}"
            }
          }
        }
      }
    }
    (trace.toList, labels.toList, edgeIds.toList)
  }
}