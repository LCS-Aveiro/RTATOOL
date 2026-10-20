package rta.backend

import rta.syntax.Program2.{QName, RxGraph}
import rta.syntax.{Condition, LtlFormula, UpdateExpr}
import scala.collection.mutable

object LtlModelChecker {

  type Result = (Boolean, List[String], List[String], Int, Boolean, List[String])

  private def clockAtoms(cond: Condition, clocks: Set[QName]): Set[Condition] = cond match {
    case c @ Condition.AtomicCond(UpdateExpr.Var(x), _, _) if clocks.contains(x) => Set(c)
    case c @ Condition.AtomicCond(_, _, UpdateExpr.Var(x)) if clocks.contains(x) => Set(c)
    case Condition.And(l, r) => clockAtoms(l, clocks) ++ clockAtoms(r, clocks)
    case Condition.Or(l, r) => clockAtoms(l, clocks) ++ clockAtoms(r, clocks)
    case _ => Set.empty
  }

  private def children(f: LtlFormula): List[LtlFormula] = f match {
    case LtlFormula.Not(p) => List(p)
    case LtlFormula.Next(p) => List(p)
    case LtlFormula.Globally(p) => List(p)
    case LtlFormula.Eventually(p) => List(p)
    case LtlFormula.And(p, q) => List(p, q)
    case LtlFormula.Or(p, q) => List(p, q)
    case LtlFormula.Impl(p, q) => List(p, q)
    case LtlFormula.Iff(p, q) => List(p, q)
    case LtlFormula.Until(p, q) => List(p, q)
    case _ => Nil
  }

  private def clockConds(f: LtlFormula, clocks: Set[QName]): Set[Condition] = f match {
    case LtlFormula.CondProp(c) => clockAtoms(c, clocks)
    case other => children(other).flatMap(clockConds(_, clocks)).toSet
  }

  private def split(rx: RxGraph, conds: List[Condition]): List[RxGraph] = conds match {
    case Nil => List(rx)
    case c :: rest =>
      val yes = RxSemantics.intersectAll(c, rx.zone, rx)
      val no = RxSemantics.intersectAll(RxSemantics.negateCondition(c), rx.zone, rx)
      (yes ++ no).flatMap(z => split(rx.copy(zone = z), rest))
  }

  private def tarjan(roots: Iterator[Int], succ: Int => Array[Int], allowed: Int => Boolean, onComponent: Set[Int] => Boolean): Unit = {
    val index = mutable.HashMap[Int, Int]()
    val low = mutable.HashMap[Int, Int]()
    val onStack = mutable.HashSet[Int]()
    val stack = mutable.Stack[Int]()
    var counter = 0
    var stop = false

    def run(root: Int): Unit = {
      val work = mutable.Stack[(Int, Int)]()
      work.push((root, 0))
      while (work.nonEmpty && !stop) {
        val (v, pos) = work.pop()
        if (pos == 0) { index(v) = counter; low(v) = counter; counter += 1; stack.push(v); onStack += v }
        val ss = succ(v)
        var i = pos
        var descended = false
        while (i < ss.length && !descended) {
          val w = ss(i)
          if (allowed(w)) {
            if (!index.contains(w)) { work.push((v, i + 1)); work.push((w, 0)); descended = true }
            else if (onStack(w)) low(v) = math.min(low(v), index(w))
          }
          if (!descended) i += 1
        }
        if (!descended) {
          if (low(v) == index(v)) {
            val comp = mutable.Set[Int]()
            var w = -1
            while (w != v) { w = stack.pop(); onStack -= w; comp += w }
            if (onComponent(comp.toSet)) stop = true
          }
          if (work.nonEmpty) { val parent = work.top._1; low(parent) = math.min(low(parent), low(v)) }
        }
      }
    }

    while (roots.hasNext && !stop) {
      val r = roots.next()
      if (allowed(r) && !index.contains(r)) run(r)
    }
  }

  def verify(start: RxGraph, formula: LtlFormula, maxStates: Int): Result = {
    val subsBuf = mutable.ArrayBuffer[LtlFormula]()
    val indexOf = mutable.Map[LtlFormula, Int]()
    def register(f: LtlFormula): Int = indexOf.get(f) match {
      case Some(i) => i
      case None =>
        children(f).foreach(register)
        subsBuf += f
        indexOf(f) = subsBuf.length - 1
        subsBuf.length - 1
    }
    val root = register(formula)
    val subs = subsBuf.toArray
    val n = subs.length
    if (n > 62) throw new RuntimeException("Fórmula LTL demasiado grande (mais de 62 subfórmulas).")
    val kids: Array[Array[Int]] = subs.map(f => children(f).map(indexOf).toArray)
    val temporal: Array[Int] = subs.indices.filter { i =>
      subs(i) match {
        case _: LtlFormula.Next | _: LtlFormula.Globally | _: LtlFormula.Eventually | _: LtlFormula.Until => true
        case _ => false
      }
    }.toArray
    if (temporal.length > 16) throw new RuntimeException("Fórmula LTL com demasiados operadores temporais (máximo 16).")

    val userClocks = start.clocks
    val conds = clockConds(formula, userClocks).toList
    val states = mutable.ArrayBuffer[RxGraph]()
    val stateIndex = mutable.HashMap[AnalyseLTS.ZoneStateKey, Int]()
    val adj = mutable.ArrayBuffer[List[(Int, String, String, Set[QName])]]()
    def keyOf(g: RxGraph) = AnalyseLTS.ZoneStateKey(g.inits, g.val_env, g.zone, g.pendingDelays, g.act)
    def idOf(g: RxGraph): Int = stateIndex.getOrElseUpdate(keyOf(g), {
      states += g; adj += Nil; states.length - 1
    })
    def withTime(g: RxGraph): RxGraph = RxSemantics.advanceTimeZone(g).getOrElse(g)
    def resetOnArrival(g: RxGraph): Set[QName] =
      userClocks.filter(c => g.zone.matrix.get((c, DBM.ZERO_CLOCK)).exists(_.value <= 0))

    val initialStates = split(withTime(start), conds).map(idOf).distinct
    var limitReached = false
    val expanded = mutable.BitSet()

    def ensureExpanded(cur: Int): Unit =
      if (!expanded(cur) && !limitReached) {
        if (states.length > maxStates) limitReached = true
        else {
          expanded += cur
          val out = for {
            (edge, next0) <- RxSemantics.nextEdgeSymbolic(states(cur)).toList.sortBy(_._1.toString)
            resets = resetOnArrival(next0)
            next <- split(withTime(next0), conds)
          } yield {
            val edgeId = if (edge._3.show == "timeout" && edge._3 != edge._4) "delay_node" else s"event_${edge._1}_${edge._2}_${edge._3}_${edge._4}"
            (idOf(next), edge._4.show, edgeId, resets)
          }
          adj(cur) = out.distinct
        }
      }

    def successors(s: Int): List[(Int, String, String, Set[QName])] = {
      ensureExpanded(s)
      if (!expanded(s)) Nil
      else if (adj(s).isEmpty) List((s, "", "", userClocks))
      else adj(s)
    }

    def atomsOf(s: Int): Array[Long] = {
      val g = states(s)
      val base = new Array[Boolean](n)
      val out = mutable.ArrayBuffer[Long]()
      var mask = 0
      val total = 1 << temporal.length
      while (mask < total) {
        var t = 0
        while (t < temporal.length) { base(temporal(t)) = ((mask >> t) & 1) == 1; t += 1 }
        var ok = true
        var i = 0
        while (i < n && ok) {
          subs(i) match {
            case LtlFormula.True => base(i) = true
            case LtlFormula.False => base(i) = false
            case LtlFormula.StateProp(e) => base(i) = g.act.exists(_._4 == e)
            case LtlFormula.CondProp(c) => base(i) = RxSemantics.evalConditionForLTL(c, g)
            case _: LtlFormula.Not => base(i) = !base(kids(i)(0))
            case _: LtlFormula.And => base(i) = base(kids(i)(0)) && base(kids(i)(1))
            case _: LtlFormula.Or => base(i) = base(kids(i)(0)) || base(kids(i)(1))
            case _: LtlFormula.Impl => base(i) = !base(kids(i)(0)) || base(kids(i)(1))
            case _: LtlFormula.Iff => base(i) = base(kids(i)(0)) == base(kids(i)(1))
            case _: LtlFormula.Eventually => if (base(kids(i)(0)) && !base(i)) ok = false
            case _: LtlFormula.Globally => if (base(i) && !base(kids(i)(0))) ok = false
            case _: LtlFormula.Until =>
              val f = base(kids(i)(0)); val g2 = base(kids(i)(1))
              if (g2 && !base(i)) ok = false
              if (base(i) && !f && !g2) ok = false
            case _: LtlFormula.Next => ()
          }
          i += 1
        }
        if (ok) {
          var bits = 0L
          var j = 0
          while (j < n) { if (base(j)) bits |= (1L << j); j += 1 }
          out += bits
        }
        mask += 1
      }
      out.toArray
    }
    val atomCache = mutable.HashMap[Int, Array[Long]]()
    def atoms(s: Int): Array[Long] = atomCache.getOrElseUpdate(s, atomsOf(s))
    def has(bits: Long, i: Int): Boolean = ((bits >> i) & 1L) == 1L

    def follows(a: Long, b: Long): Boolean = {
      var t = 0
      var ok = true
      while (t < temporal.length && ok) {
        val i = temporal(t)
        val expected = subs(i) match {
          case _: LtlFormula.Next => has(b, kids(i)(0))
          case _: LtlFormula.Eventually => has(a, kids(i)(0)) || has(b, i)
          case _: LtlFormula.Globally => has(a, kids(i)(0)) && has(b, i)
          case _: LtlFormula.Until => has(a, kids(i)(1)) || (has(a, kids(i)(0)) && has(b, i))
          case _ => has(a, i)
        }
        if (has(a, i) != expected) ok = false
        t += 1
      }
      ok
    }

    val nodeId = mutable.HashMap[(Int, Long), Int]()
    val nodeState = mutable.ArrayBuffer[Int]()
    val nodeAtom = mutable.ArrayBuffer[Long]()
    val succ = mutable.ArrayBuffer[Array[Int]]()
    val succInfo = mutable.ArrayBuffer[Array[(String, String, Set[QName])]]()
    def node(s: Int, a: Long): Int = nodeId.getOrElseUpdate((s, a), {
      nodeState += s; nodeAtom += a; succ += null; succInfo += null; nodeState.length - 1
    })
    def expand(v: Int): Unit = if (succ(v) == null) {
      val s = nodeState(v); val a = nodeAtom(v)
      val targets = mutable.ArrayBuffer[Int](); val infos = mutable.ArrayBuffer[(String, String, Set[QName])]()
      for ((s2, label, edgeId, resets) <- successors(s); b <- atoms(s2) if follows(a, b)) {
        targets += node(s2, b); infos += ((label, edgeId, resets))
      }
      succ(v) = targets.toArray; succInfo(v) = infos.toArray
    }
    def succOf(v: Int): Array[Int] = { expand(v); succ(v) }

    val initialNodes = for (s <- initialStates; a <- atoms(s).toList if !has(a, root)) yield node(s, a)

    def nonTrivial(comp: Set[Int]): Boolean = comp.size > 1 || comp.exists(v => succOf(v).contains(v))

    def selfFulfilling(comp: Set[Int]): Boolean =
      temporal.forall { i =>
        subs(i) match {
          case _: LtlFormula.Eventually => !comp.exists(v => has(nodeAtom(v), i)) || comp.exists(v => has(nodeAtom(v), kids(i)(0)))
          case _: LtlFormula.Until => !comp.exists(v => has(nodeAtom(v), i)) || comp.exists(v => has(nodeAtom(v), kids(i)(1)))
          case _: LtlFormula.Globally => comp.forall(v => has(nodeAtom(v), i)) || comp.exists(v => !has(nodeAtom(v), kids(i)(0)))
          case _ => true
        }
      }

    def boundedClock(v: Int, c: QName): Boolean =
      states(nodeState(v)).zone.matrix.get((c, DBM.ZERO_CLOCK)).exists(_.value != DBM.INF)

    def nonZenoPart(comp: Set[Int]): Option[Set[Int]] = {
      if (!nonTrivial(comp) || !selfFulfilling(comp)) return None
      val resets = mutable.Set[QName]()
      for (v <- comp; (w, i) <- succOf(v).zipWithIndex if comp.contains(w)) resets ++= succInfo(v)(i)._3
      val bad = comp.filter(v => userClocks.exists(c => !resets.contains(c) && boundedClock(v, c)))
      if (bad.isEmpty) Some(comp)
      else {
        val rest = comp -- bad
        var found: Option[Set[Int]] = None
        tarjan(rest.iterator, succOf, rest.contains, sub => { found = nonZenoPart(sub); found.isDefined })
        found
      }
    }

    def closedAtFrontier(a: Long): Boolean =
      temporal.forall { i =>
        subs(i) match {
          case _: LtlFormula.Eventually => has(a, i) && has(a, kids(i)(0))
          case _: LtlFormula.Globally => !has(a, i) && !has(a, kids(i)(0))
          case _: LtlFormula.Until =>
            if (has(a, i)) has(a, kids(i)(1)) else !has(a, kids(i)(0)) && !has(a, kids(i)(1))
          case _ => false
        }
      }

    val prefixEnd: Option[Int] = {
      val seen = mutable.HashSet[Int]() ++ initialNodes
      val q = mutable.Queue[Int]() ++ initialNodes
      var found: Option[Int] = None
      while (q.nonEmpty && found.isEmpty) {
        val v = q.dequeue()
        if (closedAtFrontier(nodeAtom(v))) found = Some(v)
        else for (w <- succOf(v) if seen.add(w)) q.enqueue(w)
      }
      found
    }

    var badComponent: Option[Set[Int]] = None
    if (prefixEnd.isEmpty)
      tarjan(initialNodes.iterator, succOf, _ => true, comp => { badComponent = nonZenoPart(comp); badComponent.isDefined })

    val numStates = states.length
    val visitLog = states.iterator.map(_.inits.mkString(",")).toList

    def describeNode(v: Int): String = {
      val g = states(nodeState(v))
      s"{${g.inits.map(_.show).mkString(",")}, act: [${g.act.map(_._4.show).mkString(", ")}]}"
    }

    def shortestPath(from: List[Int], target: Int): (Int, List[(Int, (String, String))]) = {
      val parent = mutable.Map[Int, (Int, (String, String))]()
      val seen = mutable.Set[Int]() ++ from
      val q = mutable.Queue[Int]() ++ from
      while (q.nonEmpty && !parent.contains(target) && !from.contains(target)) {
        val v = q.dequeue()
        val ss = succOf(v)
        for (i <- ss.indices if seen.add(ss(i))) { parent(ss(i)) = (v, (succInfo(v)(i)._1, succInfo(v)(i)._2)); q.enqueue(ss(i)) }
      }
      var path = List[(Int, (String, String))]()
      var cur = target
      while (parent.contains(cur)) { val (prev, info) = parent(cur); path = (cur, info) :: path; cur = prev }
      (cur, path)
    }

    (badComponent, prefixEnd) match {
      case (None, Some(end)) =>
        val (origin, steps) = shortestPath(initialNodes, end)
        val labels = mutable.ListBuffer[String](describeNode(origin))
        val ids = mutable.ListBuffer[String]()
        for ((v, (label, edgeId)) <- steps) { labels += label; ids += edgeId; labels += describeNode(v) }
        labels(labels.length - 1) = labels.last + " [VIOLAÇÃO GARANTIDA NESTE PREFIXO]"
        (false, labels.toList, ids.toList, numStates, limitReached, visitLog)
      case (None, None) => (true, Nil, Nil, numStates, limitReached, visitLog)
      case (Some(comp), _) =>
        def bfs(from: List[Int], goal: Int => Boolean, allowed: Int => Boolean): (Int, List[(Int, (String, String))]) = {
          val parent = mutable.Map[Int, (Int, (String, String))]()
          val seen = mutable.Set[Int]() ++ from
          val q = mutable.Queue[Int]() ++ from
          var found = -1
          while (q.nonEmpty && found < 0) {
            val v = q.dequeue()
            val ss = succOf(v)
            var i = 0
            while (i < ss.length && found < 0) {
              val w = ss(i)
              val info = (succInfo(v)(i)._1, succInfo(v)(i)._2)
              if (allowed(w)) {
                if (goal(w)) { parent(w) = (v, info); found = w }
                else if (seen.add(w)) { parent(w) = (v, info); q.enqueue(w) }
              }
              i += 1
            }
          }
          if (found < 0) (from.headOption.getOrElse(-1), Nil)
          else {
            var path = List[(Int, (String, String))]()
            var cur = found
            var continue = true
            while (continue) {
              val (prev, info) = parent(cur)
              path = (cur, info) :: path
              cur = prev
              continue = cur != found && parent.contains(cur)
            }
            (cur, path)
          }
        }
        val (origin, prefix) = initialNodes.find(comp.contains) match {
          case Some(entry) => (entry, Nil)
          case None => bfs(initialNodes, comp.contains, _ => true)
        }
        val first = prefix.lastOption.map(_._1).getOrElse(origin)
        val cycle = bfs(List(first), _ == first, comp.contains)._2

        val steps = prefix ++ cycle
        val labels = mutable.ListBuffer[String](describeNode(origin))
        val ids = mutable.ListBuffer[String]()
        for (((v, (label, edgeId)), i) <- steps.zipWithIndex) {
          val stutter = label.isEmpty
          val mark =
            if (i != steps.length - 1) ""
            else if (stutter) " [DEADLOCK - Semântica de Traço Finito]"
            else " [CICLO/LASSO DETETADO]"
          if (!stutter) { labels += label; ids += edgeId; labels += (describeNode(v) + mark) }
          else if (mark.nonEmpty) labels(labels.length - 1) = labels.last + mark
        }
        (false, labels.toList, ids.toList, numStates, limitReached, visitLog)
    }
  }
}
