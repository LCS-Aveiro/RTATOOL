package rta.backend

import rta.syntax.Program2.{Edges, RxGraph, QName, Edge, showEdges}
import rta.syntax.Condition
import rta.syntax.RuntimeValue
import scala.util.boundary, boundary.break
import scala.util.Random
import rta.syntax.LtlFormula
import rta.backend.LtlEvaluator

object AnalyseLTS:

  // Chaves de memoização: uma para simulação discreta, outra para análise DBM/Zonas
  case class FastStateKey(inits: Set[QName], vars: Map[QName, RuntimeValue], clocks: Map[QName, Double])
  case class ZoneStateKey(inits: Set[QName], vars: Map[QName, RuntimeValue], zone: DBM.Zone, pending: Set[(Edge, String, QName, Double)] = Set.empty)


  // ====================================================================
  // PROVA LTL EXAUSTIVA (BMC SOBRE ZONAS DBM)
  // ====================================================================
  def verifyLTLSymbolic(start: RxGraph, formula: LtlFormula, maxStates: Int = 50000, maxDepth: Int = 30): (Boolean, List[String], List[String], Int, Boolean) = {
    var counterExample: Option[(List[String], List[String])] = None
    var statesExplored = 0
    var limitReached = false

    def dfs(current: RxGraph, path: List[RxGraph], labels: List[String], edgeIds: List[String], depth: Int, visitedInPath: Set[ZoneStateKey]): Unit = {
      if (counterExample.isDefined) return
      if (statesExplored >= maxStates) {
        limitReached = true
        return
      }
      statesExplored += 1

      val key = ZoneStateKey(current.inits, current.val_env, current.zone, current.pendingDelays)
      val isCycle = visitedInPath.contains(key)
      val nexts = RxSemantics.nextEdgeSymbolic(current)
      val isDeadlock = nexts.isEmpty

      // Chegou ao fim de um ramo (Ciclo, Deadlock ou Limite de Profundidade)
      if (isCycle || isDeadlock || depth >= maxDepth) {
        val res = LtlEvaluator.eval(path, formula, LtlEvaluator.Hybrid)
        if (!res) {
          // Formata o contra-exemplo detalhado: {estado atual, act: [arestas]}
          val ceFormatted = collection.mutable.ListBuffer[String]()
          for (i <- path.indices) {
            val g = path(i)
            val inits = g.inits.map(_.show).mkString(",")
            val acts = g.act.map(_._4.show).mkString(", ")
            ceFormatted += s"{$inits, act: [$acts]}"
            
            // Adiciona a ação que ligou este estado ao próximo
            if (i < labels.length) {
              ceFormatted += labels(i)
            }
          }
          counterExample = Some((ceFormatted.toList, edgeIds))
        }
        return
      }

      // Expande todos os futuros possíveis (Matematicamente esmagados pelo DBM)
      for ((edge, nextGraph) <- nexts) {
        if (counterExample.isEmpty) {
          val label = edge._4.show
          val edgeId = s"event_${edge._1}_${edge._2}_${edge._3}_${edge._4}"
          dfs(nextGraph, path :+ nextGraph, labels :+ label, edgeIds :+ edgeId, depth + 1, visitedInPath + key)
        }
      }
    }

    dfs(start, List(start), Nil, Nil, 0, Set())

    counterExample match {
      case Some((lbls, ids)) => (false, lbls, ids, statesExplored, limitReached)
      case None => (true, Nil, Nil, statesExplored, limitReached)
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


  // ====================================================================
  // DISPATCHERS (Escolhem qual motor usar: Zonas ou Simulação Discreta)
  // ====================================================================

  def findShortestPath(start: RxGraph, targetName: QName, maxStates: Int = 3000, useZones: Boolean = false): Option[List[String]] = {
    if (useZones) findShortestPathSymbolic(start, targetName, maxStates)
    else findShortestPathDiscrete(start, targetName, maxStates)
  }

  def findShortestPathToCondition(start: RxGraph, targetCond: Condition, maxStates: Int = 50000, useZones: Boolean = false): Option[List[String]] = {
    if (useZones) findShortestPathToConditionSymbolic(start, targetCond, maxStates)
    else findShortestPathToConditionDiscrete(start, targetCond, maxStates)
  }


  // ====================================================================
  // MOTOR SIMBÓLICO (DBM / ZONAS TEMPORAIS) - Extremamente Rápido
  // ====================================================================

  private def findShortestPathSymbolic(start: RxGraph, targetName: QName, maxStates: Int): Option[List[String]] = {
    val queue = collection.mutable.Queue[(RxGraph, List[String])]()
    queue.enqueue((start, Nil))
    var visited = Set[ZoneStateKey]()
    
    while (queue.nonEmpty && visited.size < maxStates) {
      val (current, path) = queue.dequeue()
      
      if (current.inits.contains(targetName)) {
        return Some(path)
      }
      
      val key = ZoneStateKey(current.inits, current.val_env, current.zone)
      
      if (!visited.contains(key)) {
        visited += key
        
        // O nextEdgeSymbolic avalia exaustivamente intervalos de tempo infinitos
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
    
    val startKey = ZoneStateKey(start.inits, start.val_env, start.zone, start.pendingDelays)
    queue.enqueue(start)
    
    var visitedCount = 0

    while (queue.nonEmpty && visitedCount < maxStates) {
      val current = queue.dequeue()
      visitedCount += 1

      val transitions = RxSemantics.nextEdgeSymbolic(current)
      
      for ((edge, nextGraph) <- transitions) {
        val nextKey = ZoneStateKey(nextGraph.inits, nextGraph.val_env, nextGraph.zone, nextGraph.pendingDelays)
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
      currKey = ZoneStateKey(parentGraph.inits, parentGraph.val_env, parentGraph.zone, parentGraph.pendingDelays)
    }
    path.toList
  }


  // ====================================================================
  // MOTOR DISCRETO (O que já tinhas, avança com saltos numéricos delay)
  // ====================================================================

  private def findShortestPathDiscrete(start: RxGraph, targetName: QName, maxStates: Int): Option[List[String]] = {
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
            case Some(inv) => RxSemantics.evalCondition(inv, nextTimeState)
            case None => true
          }
        )
        
        if (canTimePass) List(("delay(1)", nextTimeState)) else Nil
      } else Nil

      for ((label, nextGraph) <- edgeTransitions ++ timeTransitions) {
        val nextKey = FastStateKey(nextGraph.inits, nextGraph.val_env, nextGraph.clock_env)
        
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
      currKey = FastStateKey(parentGraph.inits, parentGraph.val_env, parentGraph.clock_env)
    }
    path.toList
  }