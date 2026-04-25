package rta.backend

import rta.syntax.Program2.{Edges, RxGraph, QName, Edge, showEdges}
import rta.syntax.Condition
import scala.util.boundary, boundary.break

object AnalyseLTS:

  case class FastStateKey(inits: Set[QName], vars: Map[QName, Int], clocks: Map[QName, Double])

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


  def findShortestPath(start: RxGraph, targetName: QName, maxStates: Int = 3000): Option[List[String]] = {
    val queue = collection.mutable.Queue[(RxGraph, List[String])]()
    queue.enqueue((start, Nil))
    var visited = Set[FastStateKey]()
    
    while (queue.nonEmpty && visited.size < maxStates) {
      val (current, path) = queue.dequeue()
      
      if (current.inits.contains(targetName)) {
        return Some(path)
      }
      
      val key = FastStateKey(current.inits, current.val_env, current.clock_env)
      
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
              case Some(inv) => Condition.evaluate(inv, nextTimeState.val_env, nextTimeState.clock_env)
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

  def findShortestPathToCondition(start: RxGraph, targetCond: Condition, maxStates: Int = 50000): Option[List[String]] = boundary:
    if (Condition.evaluate(targetCond, start.val_env, start.clock_env)) break(Some(Nil))

    val queue = collection.mutable.Queue[RxGraph]()
    val parentOf = collection.mutable.Map[FastStateKey, (RxGraph, String)]()
    
    val startKey = FastStateKey(start.inits, start.val_env, start.clock_env)
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
            case Some(inv) => Condition.evaluate(inv, nextTimeState.val_env, nextTimeState.clock_env)
            case None => true
          }
        )
        
        if (canTimePass) List(("delay(1)", nextTimeState)) else Nil
      } else Nil

      for ((label, nextGraph) <- edgeTransitions ++ timeTransitions) {
        val nextKey = FastStateKey(nextGraph.inits, nextGraph.val_env, nextGraph.clock_env)
        
        if (!parentOf.contains(nextKey) && nextKey != startKey) {
          parentOf(nextKey) = (current, label)

          if (Condition.evaluate(targetCond, nextGraph.val_env, nextGraph.clock_env)) {
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
      currKey = FastStateKey(parentGraph.inits, parentGraph.val_env, parentGraph.clock_env)
    }
    path.toList
  }