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


  private def collectClockAtoms(cond: Condition, clocks: Set[QName]): Set[Condition] = cond match {
    case c @ Condition.AtomicCond(UpdateExpr.Var(x), _, _) if clocks.contains(x) => Set(c)
    case c @ Condition.AtomicCond(_, _, UpdateExpr.Var(x)) if clocks.contains(x) => Set(c)
    case Condition.And(l, r) => collectClockAtoms(l, clocks) ++ collectClockAtoms(r, clocks)
    case Condition.Or(l, r)  => collectClockAtoms(l, clocks) ++ collectClockAtoms(r, clocks)
    case _ => Set.empty
  }

  private def collectClockConds(f: LtlFormula, clocks: Set[QName]): Set[Condition] = f match {
    case LtlFormula.CondProp(c)   => collectClockAtoms(c, clocks)
    case LtlFormula.Not(p)        => collectClockConds(p, clocks)
    case LtlFormula.And(p, q)     => collectClockConds(p, clocks) ++ collectClockConds(q, clocks)
    case LtlFormula.Or(p, q)      => collectClockConds(p, clocks) ++ collectClockConds(q, clocks)
    case LtlFormula.Impl(p, q)    => collectClockConds(p, clocks) ++ collectClockConds(q, clocks)
    case LtlFormula.Iff(p, q)     => collectClockConds(p, clocks) ++ collectClockConds(q, clocks)
    case LtlFormula.Next(p)       => collectClockConds(p, clocks)
    case LtlFormula.Globally(p)   => collectClockConds(p, clocks)
    case LtlFormula.Eventually(p) => collectClockConds(p, clocks)
    case LtlFormula.Until(p, q)   => collectClockConds(p, clocks) ++ collectClockConds(q, clocks)
    case _ => Set.empty
  }

  private def splitByConditions(rx: RxGraph, conds: List[Condition]): List[RxGraph] = conds match {
    case Nil => List(rx)
    case c :: rest =>
      val trueZone  = RxSemantics.intersectConditionWithZone(c, rx.zone, rx)
      val falseZone = RxSemantics.intersectConditionWithZone(RxSemantics.negateCondition(c), rx.zone, rx)
      trueZone.toList.flatMap(z => splitByConditions(rx.copy(zone = z), rest)) ++
      falseZone.toList.flatMap(z => splitByConditions(rx.copy(zone = z), rest))
  }

  def verifyLTLSymbolic(start: RxGraph, formula: LtlFormula, requestedMaxStates: Int = 50000, requestedMaxDepth: Int = 30): (Boolean, List[String], List[String], Int, Boolean, List[String]) = {
    
    def preComputeSpace(): Option[Int] = {
      var visited = Set[ZoneStateKey]()
      var queue = List(start)
      while (queue.nonEmpty && visited.size <= requestedMaxStates) {
        val cur = queue.head
        queue = queue.tail
        val key = ZoneStateKey(cur.inits, cur.val_env, cur.zone, cur.pendingDelays, cur.act)
        if (!visited.contains(key)) {
          visited += key
          queue = queue ++ RxSemantics.nextEdgeSymbolic(cur).map(_._2)
        }
      }
      if (visited.size > requestedMaxStates) None else Some(visited.size)
    }

    val exactSizeOpt = preComputeSpace()
    val isExhaustive = exactSizeOpt.isDefined
    
    val maxStates = exactSizeOpt.getOrElse(requestedMaxStates)
    val maxDepth = exactSizeOpt.getOrElse(requestedMaxDepth)

    var counterExample: Option[(List[String], List[String])] = None
    var statesExplored = 0
    val visitLog = collection.mutable.ListBuffer[String]()

    def evalOnLassoGeneral(prefix: List[RxGraph], cycle: List[RxGraph], f: LtlFormula): Boolean = {
      val p = prefix.length
      val states = (prefix ++ cycle).toVector
      val n = states.length
      
      def succ(i: Int): Int = if (i == n - 1) p else i + 1

      def leaf(g: RxGraph, form: LtlFormula): Boolean = form match {
        case LtlFormula.StateProp(e) => g.act.exists(_._4 == e)
        case LtlFormula.CondProp(c) => RxSemantics.evalConditionForLTL(c, g)
        case _ => false
      }

      def satAt(formula: LtlFormula): Array[Boolean] = formula match {
        case LtlFormula.True  => Array.fill(n)(true)
        case LtlFormula.False => Array.fill(n)(false)
        case LtlFormula.StateProp(_) | LtlFormula.CondProp(_) =>
          Array.tabulate(n)(i => leaf(states(i), formula))
        case LtlFormula.Not(a)     => satAt(a).map(!_)
        case LtlFormula.And(a, b)  => (satAt(a), satAt(b)).zipped.map(_ && _).toArray
        case LtlFormula.Or(a, b)   => (satAt(a), satAt(b)).zipped.map(_ || _).toArray
        case LtlFormula.Impl(a, b) => (satAt(a), satAt(b)).zipped.map((x, y) => !x || y).toArray
        case LtlFormula.Iff(a, b)  => (satAt(a), satAt(b)).zipped.map(_ == _).toArray

        case LtlFormula.Next(a) =>
          val aOk = satAt(a)
          Array.tabulate(n)(i => aOk(succ(i)))

        case LtlFormula.Globally(a) =>
          val aOk = satAt(a)
          var res = Array.fill(n)(true)
          var changed = true
          while (changed) {
            val next = Array.tabulate(n)(i => aOk(i) && res(succ(i)))
            changed = !next.sameElements(res)
            res = next
          }
          res

        case LtlFormula.Eventually(a) =>
          val aOk = satAt(a)
          var res = Array.fill(n)(false)
          var changed = true
          while (changed) {
            val next = Array.tabulate(n)(i => aOk(i) || res(succ(i)))
            changed = !next.sameElements(res)
            res = next
          }
          res

        case LtlFormula.Until(a, b) =>
          val aOk = satAt(a); val bOk = satAt(b)
          var res = Array.fill(n)(false)
          var changed = true
          while (changed) {
            val next = Array.tabulate(n)(i => bOk(i) || (aOk(i) && res(succ(i))))
            changed = !next.sameElements(res)
            res = next
          }
          res
      }

      satAt(f)(0)
    }

    def isTimeDivergent(cycleStates: List[RxGraph], cycleEdgeIds: List[String]): Boolean = {
      if (start.clocks.isEmpty) return true
      if (cycleEdgeIds.contains("delay_node")) return true
      
      cycleStates.sliding(2).exists { 
        case List(a, b) => a.clocks.nonEmpty && a.zone.delay != a.zone
        case _          => false
      }
    }

    def dfs(current0: RxGraph, pathPrefix: List[RxGraph], labels: List[String], edgeIds: List[String], depth: Int, visitedInPath: Set[ZoneStateKey]): Unit = boundary {
      
      val currentWithTime = RxSemantics.advanceTimeZone(current0).getOrElse(current0)
      
      val clockConds = collectClockConds(formula, currentWithTime.clocks).toList
      val refinedStates = if (clockConds.isEmpty) List(currentWithTime) else splitByConditions(currentWithTime, clockConds)
      
      for (current <- refinedStates if counterExample.isEmpty) {
        if (statesExplored >= maxStates && !isExhaustive) break() 

        statesExplored += 1
        visitLog += current.inits.mkString(",")

        val key = ZoneStateKey(current.inits, current.val_env, current.zone, current.pendingDelays, current.act)
        val isCycle = visitedInPath.contains(key)
        val nexts = RxSemantics.nextEdgeSymbolic(current)
        val isDeadlock = nexts.isEmpty
        
        val currentPath = pathPrefix :+ current

        if (isCycle || isDeadlock || depth >= maxDepth) {
          
          var skipEvaluation = false
          if (isCycle) {
            val cycleStartIdx = currentPath.indexWhere { s =>
              ZoneStateKey(s.inits, s.val_env, s.zone, s.pendingDelays, s.act) == key
            }
            val cycleStates = currentPath.drop(cycleStartIdx)
            val cycleEdges = edgeIds.drop(cycleStartIdx)
            
            if (!isTimeDivergent(cycleStates, cycleEdges)) {
              skipEvaluation = true
            }
          }

          if (!skipEvaluation) {
            val res = if (isCycle) {
              val cycleStartIdx = currentPath.indexWhere { s =>
                ZoneStateKey(s.inits, s.val_env, s.zone, s.pendingDelays, s.act) == key
              }
              evalOnLassoGeneral(currentPath.take(cycleStartIdx), currentPath.drop(cycleStartIdx), formula)
            } else {
              LtlEvaluator.eval(currentPath, formula, LtlEvaluator.Hybrid, isSymbolic = true)
            }

            if (!res) {
              val ceFormatted = collection.mutable.ListBuffer[String]()
              for (i <- currentPath.indices) {
                val g = currentPath(i)
                val inits = g.inits.map(_.show).mkString(",")
                val acts = g.act.map(_._4.show).mkString(", ")
                
                var stateStr = s"{$inits, act: [$acts]}"
                
                if (i == currentPath.length - 1) {
                  if (isDeadlock) stateStr += " [DEADLOCK - Semântica de Traço Finito]"
                  else if (isCycle) stateStr += " [CICLO/LASSO DETETADO]"
                  else if (depth >= maxDepth) stateStr += " [LIMITE PROFUNDIDADE]"
                }
                
                ceFormatted += stateStr
                
                if (i < labels.length) {
                  ceFormatted += labels(i)
                }
              }
              counterExample = Some((ceFormatted.toList, edgeIds))
              
              break() 
            }
          }
          
        } else {
          for ((edge, nextGraph) <- nexts) {
            if (counterExample.isEmpty) {
              val label = edge._4.show
              val edgeId = if (label == "timeout") "delay_node" else s"event_${edge._1}_${edge._2}_${edge._3}_${edge._4}"
              
              dfs(nextGraph, currentPath, labels :+ label, edgeIds :+ edgeId, depth + 1, visitedInPath + key)
            }
          }
        }
      }
    }

    dfs(start, Nil, Nil, Nil, 0, Set())

    counterExample match {
      case Some((lbls, ids)) => (false, lbls, ids, statesExplored, !isExhaustive, visitLog.toList)
      case None => (true, Nil, Nil, statesExplored, !isExhaustive, visitLog.toList)
    }
  }

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
          val more = RxSemantics.nextEdge(st)
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
    if (RxSemantics.evalDataCondition(targetCond, start)) break(Some(Nil))

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

          if (RxSemantics.evalDataCondition(targetCond, nextGraph)) {
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
            queue.enqueue((nextTimeState, path :+ "delay(1)"))
          }
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
      
      val timeTransitions = if (current.clocks.nonEmpty) {
        val delayedClockEnv = current.clock_env.map { case (c, v) => (c, v + 1.0) }
        val nextTimeState = current.copy(clock_env = delayedClockEnv)
        
        val canTimePass = current.inits.forall(s => 
          nextTimeState.invariants.get(s) match {
            case Some(inv) => RxSemantics.evalCondition(inv, nextTimeState)
            case None => true
          }
        )
        
        if (canTimePass) List(("delay(1)", nextTimeState)) else Nil
      } else Nil

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