package rta.backend

import rta.syntax.Program2.{Edges, RxGraph, QName, Edge, showEdges}
import rta.syntax.Condition
import rta.syntax.UpdateExpr 
import rta.syntax.RuntimeValue
import scala.util.boundary, boundary.break
import scala.util.Random
import rta.syntax.LtlFormula
import rta.backend.LtlEvaluator

object AnalyseLTS:

  case class FastStateKey(inits: Set[QName], vars: Map[QName, RuntimeValue], clocks: Map[QName, Double], act: Edges)
  case class ZoneStateKey(inits: Set[QName], vars: Map[QName, RuntimeValue], zone: DBM.Zone, pending: Set[(Edge, String, QName, Double)], act: Edges)


  def verifyLTLSymbolic(start: RxGraph, formula: LtlFormula, requestedMaxStates: Int = 50000, requestedMaxDepth: Int = 30): (Boolean, List[String], List[String], Int, Boolean, List[String]) =
    LtlModelChecker.verify(start, formula, requestedMaxStates)

  def randomWalk(rx:RxGraph, max:Int=5000): (Set[RxGraph],Int,Edges,List[String]) =
    val states = for (a, bs) <- rx.edg.toSet; (b, id, lbl) <- bs; s <- Set(a, b) yield s
    
    def aux(next:Set[RxGraph], done:Set[RxGraph],
            nEdges:Int, fired:Edges, probs:List[String],
            limit:Int): (Set[RxGraph],Int,Edges,List[String]) =
      if limit <=0 then
        return (done,nEdges,fired, s"Reached limit - traversed +$max edges."::probs)
      
      next.headOption match
        case None =>
          val missingStates: Set[QName] =
            (rx.inits ++ fired.map(_._2)).intersect(states) -- done.flatMap(_.inits)
          
          val allPossibleEdges: Edges =
            (for (a, dests) <- rx.edg.toSet; (b, id, lbl) <- dests yield (a, b, id, lbl)) ++
            (for (a, dests) <- rx.on.toSet;  (b, id, lbl) <- dests yield (a, b, id, lbl)) ++
            (for (a, dests) <- rx.off.toSet; (b, id, lbl) <- dests yield (a, b, id, lbl))
          
          val missingEdges: Edges = allPossibleEdges -- fired
          
          if missingStates.isEmpty && missingEdges.isEmpty then
            (done, nEdges, fired, probs)
          else
            val stateErrs = if missingStates.nonEmpty 
                            then List(s"Unreachable state(s): ${missingStates.mkString(",")}") else Nil
            val edgeErrs = if missingEdges.nonEmpty  
                           then List(s"Unreachable edge(s): ${showEdges(missingEdges)}") else Nil
            (done, nEdges, fired, stateErrs ::: edgeErrs ::: probs)

        case Some(st) if done contains st =>
          aux(next-st, done, nEdges, fired, probs, limit)

        case Some(st) => 
          val more = if RxSemantics.hasTime(rx) then RxSemantics.nextEdgeSymbolic(st) else RxSemantics.nextEdge(st)
          val nEdges2 = more.size
          val newEdges = more.map(_._1)
          var incons = Set[String]()
          var moreEdges: Edges = Set()

          for e <- newEdges do
            val (toAct, toDeact, _) = RxSemantics.toOnOff(e, st)
            val fromE = RxSemantics.from(e, st)
            moreEdges = moreEdges ++ fromE
            
            val shared = toAct.intersect(toDeact)
            if shared.nonEmpty then
              val triggers = fromE -- shared
              incons = incons + s"activating and deactivating `${showEdges(shared)}` by `${showEdges(triggers)}`"

          var newProbs = probs
          if more.isEmpty then newProbs = s"Deadlock found at: ${st.inits.mkString(",")}" :: newProbs
          if incons.nonEmpty then newProbs = s"Found inconsistency: ${incons.mkString(", ")}" :: newProbs
          
          aux((next - st) ++ more.map(_._2), 
              done + st, 
              nEdges + nEdges2, 
              fired ++ newEdges ++ moreEdges, 
              newProbs, 
              limit - nEdges2)

    aux(Set(rx), Set(), 0, Set(), Nil, max)



  def findShortestPath(start: RxGraph, targetName: QName, maxStates: Int = 3000, useZones: Boolean = false): Option[List[String]] = {
    if (useZones) findShortestPathSymbolic(start, targetName, maxStates)
    else findShortestPathDiscrete(start, targetName, maxStates)
  }

  def findShortestPathToCondition(start: RxGraph, targetCond: Condition, maxStates: Int = 50000, useZones: Boolean = false): Option[List[String]] = {
    if (useZones) findShortestPathToConditionSymbolic(start, targetCond, maxStates)
    else findShortestPathToConditionDiscrete(start, targetCond, maxStates)
  }



  private def findShortestPathSymbolic(start: RxGraph, targetName: QName, maxStates: Int): Option[List[String]] = {
    val queue = collection.mutable.Queue[(RxGraph, List[String])]()
    queue.enqueue((start, Nil))
    var visited = Set[ZoneStateKey]()
    
    while (queue.nonEmpty && visited.size < maxStates) {
      val (current, path) = queue.dequeue()
      
      if (current.inits.contains(targetName)) {
        return Some(path)
      }
      
      val key = ZoneStateKey(current.inits, current.val_env, current.zone, current.pendingDelays, current.act)
      
      if (!visited.contains(key)) {
        visited += key
        
        val transitions = RxSemantics.nextEdgeSymbolic(current)
        for ((edge, nextGraph) <- transitions) {
          val label = edge._4.show
          queue.enqueue((nextGraph, path :+ label))
        }
      }
    }
    None
  }

  private def findShortestPathToConditionSymbolic(start: RxGraph, targetCond: Condition, maxStates: Int): Option[List[String]] = boundary:
    if (holdsInZone(targetCond, start)) break(Some(Nil))

    val queue = collection.mutable.Queue[RxGraph]()
    val parentOf = collection.mutable.Map[ZoneStateKey, (RxGraph, String)]()
    
    val startKey = ZoneStateKey(start.inits, start.val_env, start.zone, start.pendingDelays, start.act)
    queue.enqueue(start)
    
    var visitedCount = 0

    while (queue.nonEmpty && visitedCount < maxStates) {
      val current = queue.dequeue()
      visitedCount += 1

      val transitions = RxSemantics.nextEdgeSymbolic(current)
      
      for ((edge, nextGraph) <- transitions) {
        val nextKey = ZoneStateKey(nextGraph.inits, nextGraph.val_env, nextGraph.zone, nextGraph.pendingDelays, nextGraph.act)
        val label = edge._4.show
        
        if (!parentOf.contains(nextKey) && nextKey != startKey) {
          parentOf(nextKey) = (current, label)

          if (holdsInZone(targetCond, nextGraph)) {
            break(Some(reconstructZonePath(nextKey, parentOf)))
          }

          queue.enqueue(nextGraph)
        }
      }
    }
    None

  private def reconstructZonePath(
    targetKey: ZoneStateKey, 
    parentOf: collection.mutable.Map[ZoneStateKey, (RxGraph, String)]
  ): List[String] = {
    val path = collection.mutable.ListBuffer[String]()
    var currKey = targetKey
    
    while (parentOf.contains(currKey)) {
      val (parentGraph, label) = parentOf(currKey)
      path.prepend(label)
      currKey = ZoneStateKey(parentGraph.inits, parentGraph.val_env, parentGraph.zone, parentGraph.pendingDelays, parentGraph.act)
    }
    path.toList
  }



  private def holdsInZone(cond: Condition, g: RxGraph): Boolean =
    if (g.clocks.isEmpty) RxSemantics.evalCondition(cond, g)
    else RxSemantics.advanceTimeZone(g).exists(t => RxSemantics.intersectAll(cond, t.zone, t).nonEmpty)

  private val DelayEps = 0.001

  private def clockBounds(cond: Condition, rx: RxGraph): List[(QName, Double)] = cond match {
    case Condition.AtomicCond(UpdateExpr.Var(c), _, rhs) if rx.clocks.contains(c) =>
      List(c -> Condition.extractDouble(RxSemantics.evalExpr(rhs, rx.val_env, rx)))
    case Condition.AtomicCond(lhs, _, UpdateExpr.Var(c)) if rx.clocks.contains(c) =>
      List(c -> Condition.extractDouble(RxSemantics.evalExpr(lhs, rx.val_env, rx)))
    case Condition.And(l, r) => clockBounds(l, rx) ++ clockBounds(r, rx)
    case Condition.Or(l, r) => clockBounds(l, rx) ++ clockBounds(r, rx)
    case _ => Nil
  }

  private def showDelay(d: Double): String =
    if (d == Math.rint(d)) d.toLong.toString else BigDecimal(d).setScale(3, BigDecimal.RoundingMode.HALF_UP).bigDecimal.stripTrailingZeros.toPlainString

  private def timeSteps(rx: RxGraph): List[(String, RxGraph)] = {
    if (!RxSemantics.hasTime(rx)) return Nil
    val conds = rx.edgeConditions.values.flatten.toList ++ rx.invariants.values
    val boundGaps = conds.flatMap(clockBounds(_, rx)).flatMap { case (c, k) =>
      val d = k - rx.clock_env.getOrElse(c, 0.0)
      List(d, d + DelayEps)
    }
    val pendingGaps = rx.pendingDelays.toList.map { case (_, _, clk, target) => target - rx.clock_env.getOrElse(clk, 0.0) }
    val delays = (boundGaps ++ pendingGaps).filter(_ > 1e-9).map(d => Math.round(d * 1e6) / 1e6).distinct.sorted
    delays.flatMap(d => RxSemantics.advanceTimeBy(rx, d).map(next => (s"delay(${showDelay(d)})", next)))
  }

  private def findShortestPathDiscrete(start: RxGraph, targetName: QName, maxStates: Int): Option[List[String]] = {
    val queue = collection.mutable.Queue[(RxGraph, List[String])]()
    queue.enqueue((start, Nil))
    var visited = Set[FastStateKey]()
    
    while (queue.nonEmpty && visited.size < maxStates) {
      val (current, path) = queue.dequeue()
      
      if (current.inits.contains(targetName)) {
        return Some(path)
      }
      
      val key = FastStateKey(current.inits, current.val_env, current.clock_env, current.act)
      
      if (!visited.contains(key)) {
        visited += key
        
        val transitions = RxSemantics.nextEdge(current)
        for ((edge, nextGraph) <- transitions) {
          val label = edge._4.show
          queue.enqueue((nextGraph, path :+ label))
        }
        
        for ((label, nextTimeState) <- timeSteps(current)) {
          queue.enqueue((nextTimeState, path :+ label))
        }
      }
    }
    None
  }



  private def findShortestPathToConditionDiscrete(start: RxGraph, targetCond: Condition, maxStates: Int): Option[List[String]] = boundary:

    if (RxSemantics.evalCondition(targetCond, start)) break(Some(Nil))

    val queue = collection.mutable.Queue[RxGraph]()
    val parentOf = collection.mutable.Map[FastStateKey, (RxGraph, String)]()
    
    val startKey = FastStateKey(start.inits, start.val_env, start.clock_env, start.act)
    queue.enqueue(start)
    
    var visitedCount = 0

    while (queue.nonEmpty && visitedCount < maxStates) {
      val current = queue.dequeue()
      visitedCount += 1

      val edgeTransitions = RxSemantics.nextEdge(current).map { case (e, n) => (e._4.show, n) }
      
      val timeTransitions = timeSteps(current)

      for ((label, nextGraph) <- edgeTransitions ++ timeTransitions) {
        val nextKey = FastStateKey(nextGraph.inits, nextGraph.val_env, nextGraph.clock_env, nextGraph.act)
        
        if (!parentOf.contains(nextKey) && nextKey != startKey) {
          parentOf(nextKey) = (current, label)

          if (RxSemantics.evalCondition(targetCond, nextGraph)) {
            break(Some(reconstructFastPath(nextKey, parentOf)))
          }

          queue.enqueue(nextGraph)
        }
      }
    }
    None

  private def reconstructFastPath(
    targetKey: FastStateKey, 
    parentOf: collection.mutable.Map[FastStateKey, (RxGraph, String)]
  ): List[String] = {
    val path = collection.mutable.ListBuffer[String]()
    var currKey = targetKey
    
    while (parentOf.contains(currKey)) {
      val (parentGraph, label) = parentOf(currKey)
      path.prepend(label)
      currKey = FastStateKey(parentGraph.inits, parentGraph.val_env, parentGraph.clock_env, parentGraph.act)
    }
    path.toList
  }