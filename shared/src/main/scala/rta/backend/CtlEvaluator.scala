package rta.backend

import rta.syntax.Program2.{QName, RxGraph}
import rta.syntax.CtlFormula
import rta.syntax.CtlFormula.*
import rta.backend.AnalyseLTS.ZoneStateKey

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
      case Bind(_, p) => getConstants(p, clocks) // Hybrid
      case _ => Map.empty
    }
  }

  def verifyCTLSymbolic(start: RxGraph, formula: CtlFormula, maxStates: Int): (Boolean, Int, List[String], List[String]) = {
    // 1. Construir o Espaço de Estados Simbólico
    val states = collection.mutable.ArrayBuffer[RxGraph]()
    val stateIndices = collection.mutable.Map[ZoneStateKey, Int]()
    val adj = collection.mutable.ArrayBuffer[List[(Int, String, String)]]()

    def getKey(g: RxGraph) = ZoneStateKey(g.inits, g.val_env, g.zone, g.pendingDelays, g.act)

    states += start
    stateIndices(getKey(start)) = 0
    adj += List.empty

    var queue = List(0)
    var explored = 0

    while (queue.nonEmpty && states.length <= maxStates) {
      val currIdx = queue.head
      queue = queue.tail
      val currGraph = states(currIdx)
      explored += 1

      val nexts = RxSemantics.nextEdgeSymbolic(currGraph)
      val transitions = nexts.toList.map { case (edge, nextGraph) =>
        val key = getKey(nextGraph)
        val nextIdx = stateIndices.get(key) match {
          case Some(idx) => idx
          case None =>
            val idx = states.length
            states += nextGraph
            stateIndices(key) = idx
            adj += List.empty
            queue = queue :+ idx
            idx
        }
        val label = edge._4.show
        val edgeId = if (label == "timeout") "delay_node" else s"event_${edge._1}_${edge._2}_${edge._3}_${edge._4}"
        (nextIdx, label, edgeId)
      }
      adj(currIdx) = transitions
    }

    val N = states.length
    val adjArray = adj.map(_.map(_._1)).toArray

    // Memória para não recalcular a mesma fórmula com as mesmas variáveis
    val cache = collection.mutable.Map[(CtlFormula, Map[String, Int]), Array[Boolean]]()

    // 2. Model Checking Recursivo com Ambiente de Variáveis Híbridas (g)
    def eval(f: CtlFormula, env: Map[String, Int]): Array[Boolean] = {
      cache.getOrElseUpdate((f, env), {
        f match {
          case True => Array.fill(N)(true)
          case False => Array.fill(N)(false)
          
          case StateProp(e) =>
            val name = e.show
            if (env.contains(name)) {
              // Híbrido: É uma Variável Nominal (c == g(x))
              val boundStateIdx = env(name)
              Array.tabulate(N)(i => i == boundStateIdx)
            } else {
              // Standard: É um Nominal de Localização ou Aresta Ativa
              Array.tabulate(N)(i => states(i).act.exists(_._4 == e) || states(i).inits.contains(e))
            }
            
          case CondProp(c) => 
            Array.tabulate(N)(i => RxSemantics.evalConditionForLTL(c, states(i)))

          // Operador Hybrid Down-Arrow (↓)
          case Bind(x, p) =>
            val res = new Array[Boolean](N)
            for (i <- 0 until N) {
              // Avalia `p` sabendo que a variável `x` vale o estado `i` atual.
              res(i) = eval(p, env + (x -> i))(i)
            }
            res
          
          // Booleanos Standard
          case Not(p) => eval(p, env).map(!_)
          case And(p, q) => (eval(p, env), eval(q, env)).zipped.map(_ && _).toArray
          case Or(p, q) => (eval(p, env), eval(q, env)).zipped.map(_ || _).toArray
          case Impl(p, q) => (eval(p, env), eval(q, env)).zipped.map((a, b) => !a || b).toArray
          case Iff(p, q) => (eval(p, env), eval(q, env)).zipped.map(_ == _).toArray

          // Operadores CTL Point-Fix Iteration
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

    // Arranca a avaliação com o ambiente vazio
    val result = eval(formula, Map.empty)(0)

    var traceLabels = List[String]()
    var traceIds = List[String]()

    def extractPath(targetCond: Int => Boolean): Unit = {
      val queue = collection.mutable.Queue[Int](0)
      val parent = collection.mutable.Map[Int, (Int, String, String)]()
      val visited = collection.mutable.Set[Int](0)

      while (queue.nonEmpty) {
        val curr = queue.dequeue()
        if (targetCond(curr)) {
          var p = curr
          while (p != 0) {
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