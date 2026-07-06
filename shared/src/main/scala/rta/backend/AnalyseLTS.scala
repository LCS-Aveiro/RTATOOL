package rta.backend

import rta.syntax.Program2.{Edges, RxGraph, QName, Edge, showEdges}
import rta.syntax.Condition
import rta.syntax.RuntimeValue
import scala.util.boundary, boundary.break
import rta.syntax.Formula
import rta.syntax.Formula.*


object AnalyseLTS:

  case class FastStateKey(inits: Set[QName], vars: Map[QName, RuntimeValue], clocks: Map[QName, Double])

  // =======================================================================
  // LTL AST & EQUIVALENCE CHECKER (ReTA vs ReGA)
  // =======================================================================

  def evalReTA(trace: List[RxGraph], phi: Formula): Boolean = {
    if (trace.isEmpty) return false
    phi match {
      case True => true
      case False => false
      case StateProp(e) => trace.head.act.exists(_._4 == e) // No ReTA as folhas são Arestas Ativas
      case CondProp(_) => throw new Exception("ReTA usa semântica de aresta, não condições de variável.")
      case Not(p) => !evalReTA(trace, p)
      case And(p, q) => evalReTA(trace, p) && evalReTA(trace, q)
      case Or(p, q) => evalReTA(trace, p) || evalReTA(trace, q)
      case Impl(p, q) => !evalReTA(trace, p) || evalReTA(trace, q)
      case Iff(p, q) => evalReTA(trace, p) == evalReTA(trace, q)
      case LtlNext(p) => if (trace.tail.isEmpty) false else evalReTA(trace.tail, p)
      case LtlUntil(p, q) =>
        trace.indices.find(i => evalReTA(trace.drop(i), q)) match {
          case Some(idx) => (0 until idx).forall(i => evalReTA(trace.drop(i), p))
          case None => false
        }
      case LtlGlobally(p) => trace.indices.forall(i => evalReTA(trace.drop(i), p))
      case LtlEventually(p) => trace.indices.exists(i => evalReTA(trace.drop(i), p))
      case _ => throw new Exception(s"Operador PDL ($phi) não é suportado na avaliação linear LTL.")
    }
  }

  def evalReGA(trace: List[RxGraph], phi: Formula): Boolean = {
    if (trace.isEmpty) return false
    phi match {
      case True => true
      case False => false
      case CondProp(c) => RxSemantics.evalCondition(c, trace.head) // No ReGA as folhas são Avaliação de Variáveis
      case StateProp(_) => throw new Exception("ReGA usa variáveis (ex: [x == 1]), não arestas diretas.")
      case Not(p) => !evalReGA(trace, p)
      case And(p, q) => evalReGA(trace, p) && evalReGA(trace, q)
      case Or(p, q) => evalReGA(trace, p) || evalReGA(trace, q)
      case Impl(p, q) => !evalReGA(trace, p) || evalReGA(trace, q)
      case Iff(p, q) => evalReGA(trace, p) == evalReGA(trace, q)
      case LtlNext(p) => if (trace.tail.isEmpty) false else evalReGA(trace.tail, p)
      case LtlUntil(p, q) =>
        trace.indices.find(i => evalReGA(trace.drop(i), q)) match {
          case Some(idx) => (0 until idx).forall(i => evalReGA(trace.drop(i), p))
          case None => false
        }
      case LtlGlobally(p) => trace.indices.forall(i => evalReGA(trace.drop(i), p))
      case LtlEventually(p) => trace.indices.exists(i => evalReGA(trace.drop(i), p))
      case _ => throw new Exception(s"Operador PDL ($phi) não é suportado na avaliação linear LTL.")
    }
  }


  // Avaliador Híbrido para o Frontend (Aceita Arestas e Variáveis em simultâneo)
  def evalLTLUser(trace: List[RxGraph], phi: Formula): Boolean = {
    if (trace.isEmpty) return false
    phi match {
      case True => true
      case False => false
      case StateProp(e) => trace.head.act.exists(_._4 == e)        // Verifica Arestas
      case CondProp(c) => RxSemantics.evalCondition(c, trace.head) // Verifica Variáveis
      case Not(p) => !evalLTLUser(trace, p)
      case And(p, q) => evalLTLUser(trace, p) && evalLTLUser(trace, q)
      case Or(p, q) => evalLTLUser(trace, p) || evalLTLUser(trace, q)
      case Impl(p, q) => !evalLTLUser(trace, p) || evalLTLUser(trace, q)
      case Iff(p, q) => evalLTLUser(trace, p) == evalLTLUser(trace, q)
      case LtlNext(p) => if (trace.tail.isEmpty) false else evalLTLUser(trace.tail, p)
      case LtlUntil(p, q) =>
        trace.indices.find(i => evalLTLUser(trace.drop(i), q)) match {
          case Some(idx) => (0 until idx).forall(i => evalLTLUser(trace.drop(i), p))
          case None => false
        }
      case LtlGlobally(p) => trace.indices.forall(i => evalLTLUser(trace.drop(i), p))
      case LtlEventually(p) => trace.indices.exists(i => evalLTLUser(trace.drop(i), p))
      case _ => throw new Exception(s"Operador ($phi) não suportado em LTL Dinâmico.")
    }
  }

  def generateRandomTrace(start: RxGraph, length: Int): (List[RxGraph], List[String]) = {
    var trace = List(start)
    var labels = List[String]()
    var current = start
    for (_ <- 0 until length) {
      val nexts = RxSemantics.nextEdge(current).toList
      if (nexts.nonEmpty) {
        val step = nexts.head
        current = step._2
        trace = trace :+ current
        labels = labels :+ step._1._4.show
      }
    }
    (trace, labels)
  }

  def followPath(start: RxGraph, labels: List[String]): List[RxGraph] = {
    @scala.annotation.tailrec
    def loop(current: RxGraph, remaining: List[String], acc: List[RxGraph]): List[RxGraph] = {
      remaining match {
        case Nil => acc
        case lbl :: tail =>
          val nexts = RxSemantics.nextEdge(current).toList
          nexts.find(_._1._4.show == lbl) match {
            case Some(step) =>
              loop(step._2, tail, acc :+ step._2)
            case None =>
              acc
          }
      }
    }
    loop(start, labels, List(start))
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

  def findShortestPathToCondition(start: RxGraph, targetCond: Condition, maxStates: Int = 50000): Option[List[String]] = boundary:

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