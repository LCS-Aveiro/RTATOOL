package rta.backend

import rta.syntax.Program2.{QName, RxGraph}
import rta.syntax.CtlFormula
import rta.syntax.CtlFormula.*
import rta.backend.AnalyseLTS.ZoneStateKey
import rta.syntax.Condition
import rta.syntax.UpdateExpr

object CtlEvaluator {

  def getConstants(f: CtlFormula, clocks: Set[QName]): Map[QName, Double] = {
    import rta.backend.RxSemantics.MaxConstants.{fromCond, mergeMax}
    f match {
      case CondProp(c) => fromCond(c, clocks)
      case Not(p) => getConstants(p, clocks)
      case And(p, q) => mergeMax(getConstants(p, clocks), getConstants(q, clocks))
      case Or(p, q) => mergeMax(getConstants(p, clocks), getConstants(q, clocks))
      case Impl(p, q) => mergeMax(getConstants(p, clocks), getConstants(q, clocks))
      case Iff(p, q) => mergeMax(getConstants(p, clocks), getConstants(q, clocks))
      case EX(p) => getConstants(p, clocks)
      case EF(p) => getConstants(p, clocks)
      case EG(p) => getConstants(p, clocks)
      case EU(p, q) => mergeMax(getConstants(p, clocks), getConstants(q, clocks))
      case AX(p) => getConstants(p, clocks)
      case AF(p) => getConstants(p, clocks)
      case AG(p) => getConstants(p, clocks)
      case AU(p, q) => mergeMax(getConstants(p, clocks), getConstants(q, clocks))
      case Bind(_, p) => getConstants(p, clocks)
      case _ => Map.empty
    }
  }

  private def collectClockAtoms(cond: Condition, clocks: Set[QName]): Set[Condition] = cond match {
    case c @ Condition.AtomicCond(UpdateExpr.Var(x), _, _) if clocks.contains(x) => Set(c)
    case c @ Condition.AtomicCond(_, _, UpdateExpr.Var(x)) if clocks.contains(x) => Set(c)
    case Condition.And(l, r) => collectClockAtoms(l, clocks) ++ collectClockAtoms(r, clocks)
    case Condition.Or(l, r)  => collectClockAtoms(l, clocks) ++ collectClockAtoms(r, clocks)
    case _ => Set.empty
  }

  private def collectClockConds(f: CtlFormula, clocks: Set[QName]): Set[Condition] = f match {
    case CondProp(c)   => collectClockAtoms(c, clocks)
    case Not(p)        => collectClockConds(p, clocks)
    case And(p, q)     => collectClockConds(p, clocks) ++ collectClockConds(q, clocks)
    case Or(p, q)      => collectClockConds(p, clocks) ++ collectClockConds(q, clocks)
    case Impl(p, q)    => collectClockConds(p, clocks) ++ collectClockConds(q, clocks)
    case Iff(p, q)     => collectClockConds(p, clocks) ++ collectClockConds(q, clocks)
    case EX(p)         => collectClockConds(p, clocks)
    case EF(p)         => collectClockConds(p, clocks)
    case EG(p)         => collectClockConds(p, clocks)
    case EU(p, q)      => collectClockConds(p, clocks) ++ collectClockConds(q, clocks)
    case AX(p)         => collectClockConds(p, clocks)
    case AF(p)         => collectClockConds(p, clocks)
    case AG(p)         => collectClockConds(p, clocks)
    case AU(p, q)      => collectClockConds(p, clocks) ++ collectClockConds(q, clocks)
    case Bind(_, p)    => collectClockConds(p, clocks)
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

  def verifyCTLSymbolic(start: RxGraph, formula: CtlFormula, maxStates: Int): (Boolean, Int, List[String], List[String]) = {
    val clockConds = collectClockConds(formula, start.clocks).toList

    val states = collection.mutable.ArrayBuffer[RxGraph]()
    val stateIndices = collection.mutable.Map[ZoneStateKey, Int]()
    val adj = collection.mutable.ArrayBuffer[List[(Int, String, String)]]()

    def getKey(g: RxGraph) = ZoneStateKey(g.inits, g.val_env, g.zone, g.pendingDelays, g.act)

    val startWithTime = RxSemantics.advanceTimeZone(start).getOrElse(start)
    val initialStates = if (clockConds.isEmpty) List(startWithTime) else splitByConditions(startWithTime, clockConds)
    var queue = List.empty[Int]
    
    for (s <- initialStates) {
      val key = getKey(s)
      if (!stateIndices.contains(key)) {
        val idx = states.length
        states += s
        stateIndices(key) = idx
        adj += List.empty
        queue = queue :+ idx
      }
    }

    var explored = 0

    while (queue.nonEmpty && states.length <= maxStates) {
      val currIdx = queue.head
      queue = queue.tail
      val currGraph = states(currIdx)
      explored += 1

      val nexts = RxSemantics.nextEdgeSymbolic(currGraph)
      
      val transitions = nexts.toList.flatMap { case (edge, nextGraph0) =>
        
        val nextGraph = RxSemantics.advanceTimeZone(nextGraph0).getOrElse(nextGraph0)
        
        val refinedNexts = if (clockConds.isEmpty) List(nextGraph) else splitByConditions(nextGraph, clockConds)
        
        refinedNexts.map { refinedNext =>
          val key = getKey(refinedNext)
          val nextIdx = stateIndices.get(key) match {
            case Some(idx) => idx
            case None =>
              val idx = states.length
              states += refinedNext
              stateIndices(key) = idx
              adj += List.empty
              queue = queue :+ idx
              idx
          }
          val label = edge._4.show
          val edgeId = if (label == "timeout") "delay_node" else s"event_${edge._1}_${edge._2}_${edge._3}_${edge._4}"
          (nextIdx, label, edgeId)
        }
      }
      adj(currIdx) = transitions
    }

    val N = states.length
    val adjArray = adj.map(_.map(_._1)).toArray

    val cache = collection.mutable.Map[(CtlFormula, Map[String, Int]), Array[Boolean]]()

    def eval(f: CtlFormula, env: Map[String, Int]): Array[Boolean] = {
      cache.getOrElseUpdate((f, env), {
        f match {
          case True => Array.fill(N)(true)
          case False => Array.fill(N)(false)
          
          case StateProp(e) =>
            val name = e.show
            if (env.contains(name)) {
              val boundStateIdx = env(name)
              Array.tabulate(N)(i => i == boundStateIdx)
            } else {
              Array.tabulate(N)(i => states(i).act.exists(_._4 == e) || states(i).inits.contains(e))
            }
            
          case CondProp(c) => 
            Array.tabulate(N)(i => RxSemantics.evalConditionForLTL(c, states(i)))

          case Bind(x, p) =>
            val res = new Array[Boolean](N)
            for (i <- 0 until N) {
              res(i) = eval(p, env + (x -> i))(i)
            }
            res
          
          case Not(p) => eval(p, env).map(!_)
          case And(p, q) => (eval(p, env), eval(q, env)).zipped.map(_ && _).toArray
          case Or(p, q) => (eval(p, env), eval(q, env)).zipped.map(_ || _).toArray
          case Impl(p, q) => (eval(p, env), eval(q, env)).zipped.map((a, b) => !a || b).toArray
          case Iff(p, q) => (eval(p, env), eval(q, env)).zipped.map(_ == _).toArray

          case EX(p) =>
            val pRes = eval(p, env)
            Array.tabulate(N)(i => adjArray(i).exists(pRes))
            
          case AX(p) =>
            val pRes = eval(p, env)
            Array.tabulate(N)(i => adjArray(i).isEmpty || adjArray(i).forall(pRes))

          case EF(p) =>
            val pRes = eval(p, env)
            val res = pRes.clone()
            var changed = true
            while (changed) { changed = false
              for (i <- 0 until N) {
                if (!res(i) && adjArray(i).exists(res)) { res(i) = true; changed = true }
              }
            }
            res

          case EG(p) =>
            val pRes = eval(p, env)
            val res = pRes.clone()
            var changed = true
            while (changed) { changed = false
              for (i <- 0 until N) {
                if (res(i) && (adjArray(i).isEmpty || !adjArray(i).exists(res))) { res(i) = false; changed = true }
              }
            }
            res

          case AF(p) =>
            val pRes = eval(p, env)
            val res = pRes.clone()
            var changed = true
            while (changed) { changed = false
              for (i <- 0 until N) {
                if (!res(i) && adjArray(i).nonEmpty && adjArray(i).forall(res)) { res(i) = true; changed = true }
              }
            }
            res

          case AG(p) =>
            val pRes = eval(p, env)
            val res = pRes.clone()
            var changed = true
            while (changed) { changed = false
              for (i <- 0 until N) {
                if (res(i) && adjArray(i).exists(!res(_))) { res(i) = false; changed = true }
              }
            }
            res

          case EU(p, q) =>
            val pRes = eval(p, env); val res = eval(q, env)
            var changed = true
            while (changed) { changed = false
              for (i <- 0 until N) {
                if (!res(i) && pRes(i) && adjArray(i).exists(res)) { res(i) = true; changed = true }
              }
            }
            res

          case AU(p, q) =>
            val pRes = eval(p, env); val res = eval(q, env)
            var changed = true
            while (changed) { changed = false
              for (i <- 0 until N) {
                if (!res(i) && pRes(i) && adjArray(i).nonEmpty && adjArray(i).forall(res)) { res(i) = true; changed = true }
              }
            }
            res
        }
      })
    }

    val initialIndices = initialStates.map(s => stateIndices(getKey(s)))
    val result = initialIndices.forall(idx => eval(formula, Map.empty)(idx))

    var traceLabels = List[String]()
    var traceIds = List[String]()

    def extractPath(targetCond: Int => Boolean): Unit = {
      val queue = collection.mutable.Queue[Int](initialIndices: _*)
      val parent = collection.mutable.Map[Int, (Int, String, String)]()
      val visited = collection.mutable.Set[Int](initialIndices: _*)

      while (queue.nonEmpty) {
        val curr = queue.dequeue()
        if (targetCond(curr)) {
          var p = curr
          while (!initialIndices.contains(p)) {
            val (prev, l, e) = parent(p)
            traceLabels = l :: traceLabels
            traceIds = e :: traceIds
            p = prev
          }
          queue.clear()
        } else {
          for ((nextIdx, lbl, eid) <- adj(curr)) {
            if (!visited.contains(nextIdx)) {
              visited += nextIdx
              parent(nextIdx) = (curr, lbl, eid)
              queue.enqueue(nextIdx)
            }
          }
        }
      }
    }

    if (!result) { formula match { case AG(p) => extractPath(i => !eval(p, Map.empty)(i)); case _ => } } 
    else { formula match { case EF(p) => extractPath(i => eval(p, Map.empty)(i)); case _ => } }

    (result, explored, traceLabels, traceIds)
  }
}