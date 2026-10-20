package rta.backend

import rta.syntax.Program2.{Edge, Edges, QName, RxGraph}
import rta.syntax.{
  Condition,
  Statement,
  UpdateExpr,
  AssignStmt,
  ArrayAssignStmt,
  IfThenStmt,
  ForeachStmt,
  ReturnStmt,
  PrintStmt,
  RuntimeValue,
  FuncCallStmt,
  LocalDecl
}
import scala.annotation.tailrec
import rta.syntax.LtlFormula

object RxSemantics {

  private val EPSILON = 1e-5
  private val builtinRandom = new scala.util.Random(1)

  private val DelaySlotPrefix = "__delay"

  def isDelaySlot(q: QName): Boolean = q.n.headOption.contains(DelaySlotPrefix)

  def hasTime(rx: RxGraph): Boolean = rx.clocks.nonEmpty || rx.delays.nonEmpty || rx.pendingDelays.nonEmpty

  def delayDuration(ruleLabel: QName, rx: RxGraph): Double =
    rx.delays.get(ruleLabel).map(e => Math.max(0.0, Condition.extractDouble(evalExpr(e, rx.val_env, rx)))).getOrElse(0.0)

  def remainingTime(pending: (Edge, String, QName, Double), rx: RxGraph): Double =
    pending._4 - rx.clock_env.getOrElse(pending._3, 0.0)

  private def freshSlot(used: Set[QName]): QName =
    Iterator.from(0).map(i => QName(List(DelaySlotPrefix, i.toString))).find(q => !used.contains(q)).get

  def effectiveMax(rx: RxGraph): Map[QName, Double] =
    rx.pendingDelays.foldLeft(rx.maxConstants) { case (m, (_, _, clk, target)) =>
      m.updated(clk, Math.max(m.getOrElse(clk, 0.0), target))
    }

  object MaxConstants {
    private def fromExpr(e: UpdateExpr, clock: QName): Option[Double] = e match {
      case UpdateExpr.LitInt(i) => Some(i.toDouble)
      case UpdateExpr.LitFloat(f) => Some(f)
      case _ => Some(Double.PositiveInfinity)
    }

    def fromCond(cond: Condition, clocks: Set[QName]): Map[QName, Double] = cond match {
      case Condition.AtomicCond(UpdateExpr.Var(c), _, rhs) if clocks.contains(c) =>
        fromExpr(rhs, c).map(v => Map(c -> v)).getOrElse(Map.empty)
      case Condition.AtomicCond(lhs, _, UpdateExpr.Var(c)) if clocks.contains(c) =>
        fromExpr(lhs, c).map(v => Map(c -> v)).getOrElse(Map.empty)
      case Condition.And(l, r) => mergeMax(fromCond(l, clocks), fromCond(r, clocks))
      case Condition.Or(l, r)  => mergeMax(fromCond(l, clocks), fromCond(r, clocks))
      case _ => Map.empty
    }

    def fromLTL(f: LtlFormula, clocks: Set[QName]): Map[QName, Double] = f match {
      case LtlFormula.CondProp(c) => fromCond(c, clocks)
      case LtlFormula.Not(p) => fromLTL(p, clocks)
      case LtlFormula.And(p, q) => mergeMax(fromLTL(p, clocks), fromLTL(q, clocks))
      case LtlFormula.Or(p, q)  => mergeMax(fromLTL(p, clocks), fromLTL(q, clocks))
      case LtlFormula.Impl(p, q) => mergeMax(fromLTL(p, clocks), fromLTL(q, clocks))
      case LtlFormula.Iff(p, q)  => mergeMax(fromLTL(p, clocks), fromLTL(q, clocks))
      case LtlFormula.Next(p) => fromLTL(p, clocks)
      case LtlFormula.Globally(p) => fromLTL(p, clocks)
      case LtlFormula.Eventually(p) => fromLTL(p, clocks)
      case LtlFormula.Until(p, q) => mergeMax(fromLTL(p, clocks), fromLTL(q, clocks))
      case _ => Map.empty
    }

    def mergeMax(a: Map[QName, Double], b: Map[QName, Double]): Map[QName, Double] =
      (a.keySet ++ b.keySet).map(k => k -> Math.max(a.getOrElse(k, 0.0), b.getOrElse(k, 0.0))).toMap

    def compute(rx: RxGraph): Map[QName, Double] = {
      val clocks = rx.clocks
      var acc = Map.empty[QName, Double]
      rx.invariants.values.foreach(c => acc = mergeMax(acc, fromCond(c, clocks)))
      rx.edgeConditions.values.flatten.foreach(c => acc = mergeMax(acc, fromCond(c, clocks)))
      clocks.foreach(c => if (!acc.contains(c)) acc += (c -> 0.0))
      acc
    }
  }


  private def evalBuiltinFunc(
    funcName: QName,
    args: List[UpdateExpr],
    env: Map[QName, RuntimeValue],
    rx: RxGraph
  ): Option[RuntimeValue] = {
    val name = funcName.n.lastOption.getOrElse("")

    val argVals = args.map(a => evalExpr(a, env, rx))

    def doubleAt(i: Int): Option[Double] =
      argVals.lift(i).map(Condition.extractDouble)

    def anyFloat: Boolean =
      argVals.exists(_.isInstanceOf[RuntimeValue.VFloat])

    def numeric(v: Double): RuntimeValue =
      if (anyFloat) RuntimeValue.VFloat(v)
      else RuntimeValue.VInt(v.toInt)

    name match {

      case "floor" if args.size == 1 =>
        doubleAt(0).map(v => RuntimeValue.VInt(Math.floor(v).toInt))

      case "ceil" if args.size == 1 =>
        doubleAt(0).map(v => RuntimeValue.VInt(Math.ceil(v).toInt))

      case "round" if args.size == 1 =>
        doubleAt(0).map(v => RuntimeValue.VInt(Math.round(v).toInt))

      case "abs" if args.size == 1 =>
        doubleAt(0).map(v => numeric(Math.abs(v)))

      case "sqrt" if args.size == 1 =>
        doubleAt(0).map(v => RuntimeValue.VFloat(Math.sqrt(Math.max(0.0, v))))

      case "pow" if args.size == 2 =>
        for {
          x <- doubleAt(0)
          y <- doubleAt(1)
        } yield RuntimeValue.VFloat(Math.pow(x, y))

      case "min" if args.size == 2 =>
        for {
          a <- doubleAt(0)
          b <- doubleAt(1)
        } yield numeric(Math.min(a, b))

      case "max" if args.size == 2 =>
        for {
          a <- doubleAt(0)
          b <- doubleAt(1)
        } yield numeric(Math.max(a, b))

      case "mod" if args.size == 2 =>
        for {
          a <- doubleAt(0)
          b <- doubleAt(1)
        } yield {
          if (b == 0.0) RuntimeValue.VInt(0)
          else numeric(a % b)
        }

      case "clamp" if args.size == 3 =>
        for {
          x <- doubleAt(0)
          lo <- doubleAt(1)
          hi <- doubleAt(2)
        } yield {
          val low = Math.min(lo, hi)
          val high = Math.max(lo, hi)
          numeric(Math.max(low, Math.min(high, x)))
        }

      case "random" if args.size == 2 =>
        for {
          lower <- doubleAt(0)
          upper <- doubleAt(1)
        } yield {
          val lo = Math.min(lower, upper)
          val hi = Math.max(lower, upper)
          RuntimeValue.VFloat(lo + builtinRandom.nextDouble() * (hi - lo))
        }

      case _ =>
        None
    }
  }

  def evalExpr(expr: UpdateExpr, env: Map[QName, RuntimeValue], rx: RxGraph): RuntimeValue = expr match {
    case UpdateExpr.LitInt(i) => RuntimeValue.VInt(i)
    case UpdateExpr.LitFloat(f) => RuntimeValue.VFloat(f)
    case UpdateExpr.LitBool(b) => RuntimeValue.VBool(b)
    case UpdateExpr.LitArray(elems) => RuntimeValue.VArray(elems.map(e => evalExpr(e, env, rx)), isDynamic = true)
    case UpdateExpr.Var(q) => 
      if (rx.clock_env.contains(q)) RuntimeValue.VFloat(rx.clock_env(q))
      else env.get(q).orElse {
        if (q.n.size > 1) {
           val globalName = QName(List(q.n.last))
           rx.clock_env.get(globalName).map(RuntimeValue.VFloat(_)).orElse(env.get(globalName))
        } else None
      }.getOrElse(RuntimeValue.VInt(0)) 
    case UpdateExpr.ArrayAccess(arrName, idxExpr) =>
      val idx = Condition.extractDouble(evalExpr(idxExpr, env, rx)).toInt
      env.get(arrName) match {
        case Some(RuntimeValue.VArray(elems, _, _)) if idx >= 0 && idx < elems.size => elems(idx)
        case _ => RuntimeValue.VInt(0)
      }
    case UpdateExpr.MathOp(l, op, r) =>
      val leftVal = evalExpr(l, env, rx)
      val rightVal = evalExpr(r, env, rx)
      val isFloat = leftVal.isInstanceOf[RuntimeValue.VFloat] || rightVal.isInstanceOf[RuntimeValue.VFloat]
      val lD = Condition.extractDouble(leftVal)
      val rD = Condition.extractDouble(rightVal)
      op match {
        case "+" => if(isFloat) RuntimeValue.VFloat(lD + rD) else RuntimeValue.VInt((lD + rD).toInt)
        case "-" => if(isFloat) RuntimeValue.VFloat(lD - rD) else RuntimeValue.VInt((lD - rD).toInt)
        case "*" => if(isFloat) RuntimeValue.VFloat(lD * rD) else RuntimeValue.VInt((lD * rD).toInt)
        case "/" => if(rD != 0) { if(isFloat) RuntimeValue.VFloat(lD / rD) else RuntimeValue.VInt((lD / rD).toInt) } else RuntimeValue.VInt(0)
        case _ => RuntimeValue.VInt(0)
      }
    case UpdateExpr.FuncCall(funcName, args) =>
      evalBuiltinFunc(funcName, args, env, rx).getOrElse {
        rx.functions.get(funcName) match {
          case Some(funcDef) =>
            val evalArgs = args.map(a => evalExpr(a, env, rx))

            var localEnv = env
            funcDef.params.zip(evalArgs).foreach { case (param, v) =>
              localEnv += (param -> v)
            }

            val (finalEnv, _) = applyUpdates(funcDef.body, rx.copy(val_env = localEnv))
            finalEnv.getOrElse(QName(List("__return")), RuntimeValue.VInt(0))

          case None =>
            RuntimeValue.VInt(0)
        }
      }
  }


  def evalCondition(cond: Condition, rx: RxGraph): Boolean = cond match {
    case Condition.AtomicCond(l, op, r) =>
      Condition.compareValues(evalExpr(l, rx.val_env, rx), op, evalExpr(r, rx.val_env, rx))
    case Condition.And(l, r) => evalCondition(l, rx) && evalCondition(r, rx)
    case Condition.Or(l, r) => evalCondition(l, rx) || evalCondition(r, rx)
  }

  def evalConditionForLTL(cond: Condition, rx: RxGraph): Boolean = cond match {
    case Condition.AtomicCond(UpdateExpr.Var(c), _, _) if rx.clocks.contains(c) =>
      intersectAll(cond, rx.zone, rx).nonEmpty
    case Condition.AtomicCond(_, _, UpdateExpr.Var(c)) if rx.clocks.contains(c) =>
      intersectAll(cond, rx.zone, rx).nonEmpty
    case Condition.AtomicCond(l, op, r) =>
      Condition.compareValues(evalExpr(l, rx.val_env, rx), op, evalExpr(r, rx.val_env, rx))
    case Condition.And(l, r) => evalConditionForLTL(l, rx) && evalConditionForLTL(r, rx)
    case Condition.Or(l, r) => evalConditionForLTL(l, rx) || evalConditionForLTL(r, rx)
  }

  def negateCondition(cond: Condition): Condition = cond match {
    case Condition.AtomicCond(l, op, r) =>
      val invOp = op match {
        case "<" => ">="; case "<=" => ">"; case ">" => "<="; case ">=" => "<"
        case "==" | "=" => "!="; case "!=" => "=="; case _ => op
      }
      Condition.AtomicCond(l, invOp, r)
    case Condition.And(l, r) => Condition.Or(negateCondition(l), negateCondition(r))
    case Condition.Or(l, r) => Condition.And(negateCondition(l), negateCondition(r))
  }

  def hasClockCondition(cond: Condition, clocks: Set[QName]): Boolean = cond match {
    case Condition.AtomicCond(UpdateExpr.Var(c), _, _) if clocks.contains(c) => true
    case Condition.AtomicCond(_, _, UpdateExpr.Var(c)) if clocks.contains(c) => true
    case Condition.And(l, r) => hasClockCondition(l, clocks) || hasClockCondition(r, clocks)
    case Condition.Or(l, r) => hasClockCondition(l, clocks) || hasClockCondition(r, clocks)
    case _ => false
  }

  @tailrec
  private def cascade(pending: Set[QName], done: Set[Edge])(using rx: RxGraph): Edges = {
    if (pending.isEmpty) done
    else {
      val curr = pending.head
      val rulesOn = rx.on.getOrElse(curr, Set.empty).map(t => (curr, t._1, t._2, t._3))
      val rulesOff = rx.off.getOrElse(curr, Set.empty).map(t => (curr, t._1, t._2, t._3))
      val newRules = (rulesOn ++ rulesOff).filter(rx.act.contains) -- done
      cascade(pending.tail ++ newRules.map(_._4).filter(_.n.nonEmpty), done ++ newRules)
    }
  }

  def applyUpdates(
    stmts: List[Statement],
    rx: RxGraph
  ): (Map[QName, RuntimeValue], Map[QName, Double]) = {
    val (env, clockEnv, _) = applyUpdatesFull(stmts, rx)
    (env, clockEnv)
  }

  def applyUpdatesFull(
    stmts: List[Statement],
    rx: RxGraph
  ): (Map[QName, RuntimeValue], Map[QName, Double], Set[QName]) = {

    var assignedClocks = Set.empty[QName]
    var currentEnv = rx.val_env
    var currentClockEnv = rx.clock_env

    val returnKey = QName(List("__return"))

    var localScopes: List[scala.collection.mutable.Set[QName]] =
      List(scala.collection.mutable.Set.empty[QName])


    var scopeSaves: List[scala.collection.mutable.Map[QName, Option[RuntimeValue]]] =
      List(scala.collection.mutable.Map.empty[QName, Option[RuntimeValue]])

    def pushScope(): Unit = {
      localScopes = scala.collection.mutable.Set.empty[QName] :: localScopes
      scopeSaves = scala.collection.mutable.Map.empty[QName, Option[RuntimeValue]] :: scopeSaves
    }

    def declareLocal(q: QName): Unit = {
      localScopes.headOption.foreach { scope =>
        if (!scope.contains(q)) {
          scope += q

          scopeSaves.headOption.foreach { saves =>
            if (!saves.contains(q)) {
              saves(q) = currentEnv.get(q)
            }
          }
        }
      }
    }

    def popScope(): Unit = {
      if (localScopes.nonEmpty) {
        val saves = scopeSaves.headOption.getOrElse(
          scala.collection.mutable.Map.empty[QName, Option[RuntimeValue]]
        )

        localScopes = localScopes.tail
        scopeSaves = scopeSaves.tail

        saves.foreach {
          case (q, Some(oldValue)) =>
            currentEnv += (q -> oldValue)

          case (q, None) =>
            currentEnv -= q
        }
      }
    }

    def assignWithBounds(q: QName, newVal: RuntimeValue): Unit = {
      val existing = currentEnv.get(q)

      existing match {
        case Some(RuntimeValue.VInt(_, minOpt, maxOpt)) =>
          val v = newVal match {
            case RuntimeValue.VInt(i, _, _)     => i
            case RuntimeValue.VFloat(f, _, _)   => f.toInt
            case _                              => 0
          }

          val cappedMin = minOpt.map(m => Math.max(m, v)).getOrElse(v)
          val finalVal = maxOpt.map(m => Math.min(m, cappedMin)).getOrElse(cappedMin)

          currentEnv += (q -> RuntimeValue.VInt(finalVal, minOpt, maxOpt))

        case Some(RuntimeValue.VFloat(_, minOpt, maxOpt)) =>
          val v = newVal match {
            case RuntimeValue.VFloat(f, _, _)   => f
            case RuntimeValue.VInt(i, _, _)     => i.toDouble
            case _                              => 0.0
          }

          val cappedMin = minOpt.map(m => Math.max(m, v)).getOrElse(v)
          val finalVal = maxOpt.map(m => Math.min(m, cappedMin)).getOrElse(cappedMin)

          currentEnv += (q -> RuntimeValue.VFloat(finalVal, minOpt, maxOpt))

        case _ =>
          currentEnv += (q -> newVal)
      }
    }

    def process(ss: List[Statement]): Unit = {
      val it = ss.iterator

      while (it.hasNext && !currentEnv.contains(returnKey)) {
        it.next() match {

          case AssignStmt(v, expr) =>
            val evaluated = evalExpr(
              expr,
              currentEnv,
              rx.copy(clock_env = currentClockEnv)
            )

            if (rx.clocks.contains(v)) {
              currentClockEnv += (v -> Math.max(0.0, Condition.extractDouble(evaluated)))
              assignedClocks += v
            } else {
              assignWithBounds(v, evaluated)
            }

          case ArrayAssignStmt(arrName, idxExpr, valExpr) =>
            val idx = Condition
              .extractDouble(
                evalExpr(idxExpr, currentEnv, rx.copy(clock_env = currentClockEnv))
              )
              .toInt

            val value = evalExpr(
              valExpr,
              currentEnv,
              rx.copy(clock_env = currentClockEnv)
            )

            currentEnv.get(arrName) match {
              case Some(RuntimeValue.VArray(elems, isDyn, maxOpt)) =>
                if (idx >= 0 && idx < elems.length) {
                  currentEnv += (
                    arrName -> RuntimeValue.VArray(
                      elems.updated(idx, value),
                      isDyn,
                      maxOpt
                    )
                  )
                } else if (isDyn && idx == elems.length) {
                  val newElems = elems :+ value
                  currentEnv += (
                    arrName -> RuntimeValue.VArray(
                      maxOpt.map(m => newElems.takeRight(m)).getOrElse(newElems),
                      isDyn,
                      maxOpt
                    )
                  )
                }

              case _ =>
            }

          case IfThenStmt(cond, thens) =>
            if (
              evalCondition(
                cond,
                rx.copy(val_env = currentEnv, clock_env = currentClockEnv)
              )
            ) {
              process(thens)
            }

          case ForeachStmt(iter, arr, body) =>
            currentEnv.get(arr) match {
              case Some(RuntimeValue.VArray(elems, _, _)) =>
                val eIt = elems.iterator

                while (eIt.hasNext && !currentEnv.contains(returnKey)) {
                  currentEnv += (iter -> eIt.next())
                  process(body)
                }

                currentEnv -= iter

              case _ =>
            }

          case LocalDecl(typeName, v, expr) =>
            declareLocal(v)

            currentEnv -= v

            val evaluated = evalExpr(
              expr,
              currentEnv,
              rx.copy(clock_env = currentClockEnv)
            )

            val typedValue = typeName match {
              case "int" =>
                RuntimeValue.VInt(Condition.extractDouble(evaluated).toInt)

              case "float" =>
                RuntimeValue.VFloat(Condition.extractDouble(evaluated))

              case "bool" =>
                RuntimeValue.VBool(Condition.extractDouble(evaluated) != 0.0)

              case _ =>
                evaluated
            }

            assignWithBounds(v, typedValue)

          case ReturnStmt(expr) =>
            currentEnv += (
              returnKey -> evalExpr(
                expr,
                currentEnv,
                rx.copy(clock_env = currentClockEnv)
              )
            )

          case PrintStmt(expr) =>
            val evaluated = evalExpr(
              expr,
              currentEnv,
              rx.copy(clock_env = currentClockEnv)
            )

            println(
              s"🖨️ RTA Print | ${UpdateExpr.show(expr)} = ${evaluated.value}"
            )

          case FuncCallStmt(funcName, args) =>
            val builtin = evalBuiltinFunc(
              funcName,
              args,
              currentEnv,
              rx.copy(clock_env = currentClockEnv)
            )

            if (builtin.isEmpty) {
              rx.functions.get(funcName) match {
                case Some(funcDef) =>
                  val evalArgs = args.map(
                    a => evalExpr(a, currentEnv, rx.copy(clock_env = currentClockEnv))
                  )

                  pushScope()

                  funcDef.params.foreach(declareLocal)

                  funcDef.params.zip(evalArgs).foreach { case (param, v) =>
                    currentEnv += (param -> v)
                  }

                  process(funcDef.body)

                  currentEnv -= returnKey

                  popScope()

                case None =>
                  ()
              }
            }
        }
      }
    }

    process(stmts)

    popScope()

    (currentEnv, currentClockEnv, assignedClocks)
  }

  def from(e: Edge, rx: RxGraph): Set[Edge] = cascade(Set(e._4), Set())(using rx)

  private def getHyperEdgeEffects(e: Edge, rx: RxGraph, evalFn: (Condition, RxGraph) => Boolean = evalCondition): (Edges, Edges, List[Statement], Set[(Edge, String, QName, Double)]) = {
    val triggeredHyperEdges = from(e, rx)
    var toActivate = Set.empty[Edge]
    var toDeactivate = Set.empty[Edge]
    var updatesToApply = List.empty[Statement]
    var newPending = Set.empty[(Edge, String, QName, Double)]

    for (hyperEdge <- triggeredHyperEdges) {
      if (rx.act.contains(hyperEdge)) {
        val conditionHolds = rx.edgeConditions.getOrElse(hyperEdge, None).forall(c => evalFn(c, rx))
        if (conditionHolds) {
          updatesToApply = updatesToApply ::: rx.edgeUpdates.getOrElse(hyperEdge, Nil)
          val (triggerLabel, targetLabel, ruleId, ruleLabel) = hyperEdge
          val isOn = rx.on.getOrElse(triggerLabel, Set.empty).contains((targetLabel, ruleId, ruleLabel))
          val isOff = rx.off.getOrElse(triggerLabel, Set.empty).contains((targetLabel, ruleId, ruleLabel))
          
          if (rx.delays.contains(ruleLabel)) {
            val delayVal = delayDuration(ruleLabel, rx)
            def slot() = freshSlot(rx.pendingDelays.map(_._3) ++ newPending.map(_._3))
            if (isOn) newPending += ((hyperEdge, "on", slot(), delayVal))
            if (isOff) newPending += ((hyperEdge, "off", slot(), delayVal))
          } else {
            if (isOn) toActivate = toActivate ++ rx.lbls.getOrElse(targetLabel, Set.empty)
            if (isOff) toDeactivate = toDeactivate ++ rx.lbls.getOrElse(targetLabel, Set.empty)
          }
        }
      }
    }
    (toActivate, toDeactivate, updatesToApply, newPending)
  }

  def evalDataCondition(cond: Condition, rx: RxGraph): Boolean = cond match {
    case Condition.AtomicCond(UpdateExpr.Var(c), _, _) if rx.clocks.contains(c) => true
    case Condition.AtomicCond(_, _, UpdateExpr.Var(c)) if rx.clocks.contains(c) => true
    case Condition.AtomicCond(l, op, r) => Condition.compareValues(evalExpr(l, rx.val_env, rx), op, evalExpr(r, rx.val_env, rx))
    case Condition.And(l, r) => evalDataCondition(l, rx) && evalDataCondition(r, rx)
    case Condition.Or(l, r) => evalDataCondition(l, rx) || evalDataCondition(r, rx)
  }

  def toOnOff(e: Edge, rx0: RxGraph): (Edges, Edges, Map[QName, RuntimeValue]) = {
    val rx = applyTimeouts(rx0)
    val (toA, toD, stmts, _) = getHyperEdgeEffects(e, rx)
    val (nextEnv, _) = applyUpdates(stmts, rx)
    (toA, toD, nextEnv)
  }

  private def checkInvariant(state: QName, rx: RxGraph): Boolean = {
    rx.invariants.get(state) match {
      case Some(inv) => evalCondition(inv, rx)
      case None => true
    }
  }

  def nextEdgeSymbolic(rx0: RxGraph): Set[(Edge, RxGraph)] = {
    val rxTimeOpt = advanceTimeZone(rx0)
    if (rxTimeOpt.isEmpty) return Set.empty
    val rx = rxTimeOpt.get

    def splitZones(heList: List[Edge], currentZ: DBM.Zone): List[(DBM.Zone, Set[Edge])] = heList match {
      case Nil => List((currentZ, Set.empty))
      case he :: tail =>
        rx.edgeConditions.getOrElse(he, None) match {
          case None =>
            splitZones(tail, currentZ).map { case (fZ, fHE) => (fZ, fHE + he) }
          case Some(c) if !hasClockCondition(c, rx.clocks) =>
            if (evalCondition(c, rx)) splitZones(tail, currentZ).map { case (fZ, fHE) => (fZ, fHE + he) }
            else splitZones(tail, currentZ)
          case Some(c) =>
            val resTrue = intersectAll(c, currentZ, rx).flatMap(z => splitZones(tail, z).map { case (fZ, fHE) => (fZ, fHE + he) })
            val resFalse = intersectAll(negateCondition(c), currentZ, rx).flatMap(z => splitZones(tail, z))
            resTrue ++ resFalse
        }
    }

    val transitions = for {
      st <- rx.inits.toList
      (st2, tid, lbl) <- rx.edg.getOrElse(st, Set.empty).toList
      edge = (st, st2, tid, lbl)
      if rx.act.contains(edge)
      guardZone <- (rx.edgeConditions.getOrElse(edge, None) match {
        case Some(cond) => intersectAll(cond, rx.zone, rx)
        case None => List(rx.zone)
      })
      triggeredHyperEdges = from(edge, rx).filter(rx.act.contains).toList.sortBy(_.toString)
      (finalZone, firedHEs) <- splitZones(triggeredHyperEdges, guardZone)
    } yield {
      var toAct = Set.empty[Edge]
      var toDeact = Set.empty[Edge]
      var updatesToApply = List.empty[Statement]
      var newPending = Set.empty[(Edge, String, QName, Double)]

      for (hyperEdge <- triggeredHyperEdges if firedHEs.contains(hyperEdge)) {
        updatesToApply = updatesToApply ::: rx.edgeUpdates.getOrElse(hyperEdge, Nil)
        val (triggerLabel, targetLabel, ruleId, ruleLabel) = hyperEdge

        val isOn = rx.on.getOrElse(triggerLabel, Set.empty).contains((targetLabel, ruleId, ruleLabel))
        val isOff = rx.off.getOrElse(triggerLabel, Set.empty).contains((targetLabel, ruleId, ruleLabel))

        if (rx.delays.contains(ruleLabel)) {
          val delayVal = delayDuration(ruleLabel, rx)
          def slot() = freshSlot(rx.pendingDelays.map(_._3) ++ newPending.map(_._3))
          if (isOn) newPending += ((hyperEdge, "on", slot(), delayVal))
          if (isOff) newPending += ((hyperEdge, "off", slot(), delayVal))
        } else {
          if (isOn) toAct = toAct ++ rx.lbls.getOrElse(targetLabel, Set.empty)
          if (isOff) toDeact = toDeact ++ rx.lbls.getOrElse(targetLabel, Set.empty)
        }
      }

      val currentAct = (rx.act ++ toAct) -- toDeact
      val allStmts = rx.edgeUpdates.getOrElse(edge, Nil) ++ updatesToApply
      val (nextEnv, nextClockEnv, assignedClocks) = applyUpdatesFull(allStmts, rx)

      val zoneAfterAssign = assignedClocks.toList.sortBy(_.toString)
        .foldLeft(finalZone)((z, c) => z.assign(c, nextClockEnv.getOrElse(c, 0.0)))
      val zoneWithDelays = newPending.map(_._3).toList.sortBy(_.toString)
        .foldLeft(zoneAfterAssign)(_.withClock(_))

      val next = rx.copy(
        inits = (rx.inits - st) + st2,
        act = currentAct,
        val_env = nextEnv,
        clock_env = nextClockEnv,
        pendingDelays = rx.pendingDelays ++ newPending
      )
      (edge, next.copy(zone = zoneWithDelays.extrapolate(effectiveMax(next))))
    }

    val standardTransitions = transitions.filter { case (_, nextRx) =>
      nextRx.inits.forall(s => nextRx.invariants.get(s) match {
        case Some(inv) => intersectAll(inv, nextRx.zone, nextRx).nonEmpty
        case None => true
      })
    }.toSet

    val timeoutTransitions = for {
      pending <- rx.pendingDelays
      (hyperEdge, opType, clock, targetVal) = pending
      if rx.zone.clocks.contains(clock)
      maturedZoneOpt = rx.zone.constrain(DBM.ZERO_CLOCK, clock, -targetVal, isStrict = false)
      if maturedZoneOpt.isDefined
    } yield {
      val remaining = rx.pendingDelays - pending
      val z0 = maturedZoneOpt.get
      val z1 = z0.withoutClock(clock)
      val targetLabel = hyperEdge._2
      val affectedEdges = rx.lbls.getOrElse(targetLabel, Set.empty)

      val newAct = if (opType == "on") rx.act ++ affectedEdges else rx.act -- affectedEdges

      val next = rx.copy(
        act = newAct,
        clock_env = rx.clock_env - clock,
        pendingDelays = remaining
      )
      val nextRx = next.copy(zone = z1.extrapolate(effectiveMax(next)))

      val ruleLabel = hyperEdge._4
      val pseudoEdge: Edge = (
        rx.inits.headOption.getOrElse(QName(Nil)),
        rx.inits.headOption.getOrElse(QName(Nil)),
        QName(List("timeout")),
        ruleLabel
      )
      (pseudoEdge, nextRx)
    }

    standardTransitions ++ timeoutTransitions
  }

  def nextEdge(rx0: RxGraph): Set[(Edge, RxGraph)] = {
    val rxTimeOpt = advanceTimeZone(rx0)
    if (rxTimeOpt.isEmpty) return Set.empty
    val rx = rxTimeOpt.get

    val transitions = for {
      st <- rx.inits
      (st2, tid, lbl) <- rx.edg.getOrElse(st, Set.empty)
      edge = (st, st2, tid, lbl)
      if rx.act.contains(edge)
    } yield {
      val condOpt = rx.edgeConditions.getOrElse(edge, None)
      val zoneAfterGuard = condOpt match {
        case Some(cond) => intersectAll(cond, rx.zone, rx).headOption
        case None => Some(rx.zone)
      }

      if (zoneAfterGuard.isDefined && condOpt.forall(c => evalCondition(c, rx))) {
        val (toAct, toDeact, hStmts, relativePending) = getHyperEdgeEffects(edge, rx)
        val currentAct = (rx.act ++ toAct) -- toDeact
        val allStmts = rx.edgeUpdates.getOrElse(edge, Nil) ++ hStmts
        val (nextEnv, nextClockEnv, assignedClocks) = applyUpdatesFull(allStmts, rx)
        
        val newSlots = relativePending.toList.map(_._3).sortBy(_.toString)

        val zoneAfterUpdates = assignedClocks.toList.sortBy(_.toString)
          .foldLeft(zoneAfterGuard.get)((z, c) => z.assign(c, nextClockEnv.getOrElse(c, 0.0)))

        Some((edge, rx.copy(
          inits = (rx.inits - st) + st2,
          act = currentAct,
          val_env = nextEnv,
          clock_env = nextClockEnv ++ newSlots.map(_ -> 0.0),
          zone = newSlots.foldLeft(zoneAfterUpdates)(_.withClock(_)),
          pendingDelays = rx.pendingDelays ++ relativePending
        )))
      } else None
    }

    transitions.flatten.filter { case (_, nextRx) =>
      nextRx.inits.forall(s => nextRx.invariants.get(s) match {
        case Some(inv) => intersectAll(inv, nextRx.zone, nextRx).nonEmpty
        case None => true
      })
    }
  }

  def nextDelay(rx0: RxGraph): Set[(QName, RxGraph)] = {
    val rx = applyTimeouts(rx0)
    if (!hasTime(rx)) return Set.empty
    val step = 0.000001 
        
    val canPass = rx.pendingDelays.forall { case (_, _, clock, targetVal) => 
        rx.clock_env.getOrElse(clock, 0.0) + step <= targetVal + EPSILON
    }
    if (!canPass) return Set.empty

    val delayedClockEnv = rx.clock_env.map { case (c, v) => (c, v + step) }
    val potentialNextRx = applyTimeouts(rx.copy(clock_env = delayedClockEnv))
    
    if (rx.inits.forall(s => checkInvariant(s, potentialNextRx))) 
        Set((QName(List("delay")), potentialNextRx))
    else Set.empty
  }

  @tailrec
  def applyTimeouts(rx: RxGraph): RxGraph = {
    val matured = rx.pendingDelays.filter { case (_, _, clock, targetVal) => 
       rx.clock_env.getOrElse(clock, 0.0) >= targetVal - EPSILON
    }
    if (matured.nonEmpty) {
       var toAct = Set.empty[Edge]
       var toDeact = Set.empty[Edge]
       for (m <- matured) {
          val (hyperEdge, opType, _, _) = m
          val targetLabel = hyperEdge._2
          if (opType == "on") toAct = toAct ++ rx.lbls.getOrElse(targetLabel, Set.empty)
          if (opType == "off") toDeact = toDeact ++ rx.lbls.getOrElse(targetLabel, Set.empty)
       }
       val slots = matured.map(_._3)
       val nextRx = rx.copy(
         act = (rx.act ++ toAct) -- toDeact, 
         clock_env = rx.clock_env -- slots,
         zone = slots.foldLeft(rx.zone)(_.withoutClock(_)),
         pendingDelays = rx.pendingDelays -- matured
       )
       applyTimeouts(nextRx)
    } else rx
  }

  @tailrec
  def advanceTimeBy(rx0: RxGraph, delay: Double): Option[RxGraph] = {
    if (delay <= 0) return Some(rx0)
    val rx = applyTimeouts(rx0)
    val gaps = rx.pendingDelays.toList
      .map(remainingTime(_, rx))
      .filter(g => g > EPSILON && g < delay - EPSILON)
    if (gaps.isEmpty) advanceExactly(rx, delay)
    else {
      val first = gaps.min
      advanceExactly(rx, first) match {
        case Some(mid) => advanceTimeBy(mid, delay - first)
        case None => None
      }
    }
  }

  private def advanceExactly(rx: RxGraph, delay: Double): Option[RxGraph] = {
    val delayedClockEnv = rx.clock_env.map { case (c, v) => (c, v + delay) }
    val potentialNextRx = rx.copy(clock_env = delayedClockEnv)
    val invariantsHold = rx.inits.forall(s => 
      potentialNextRx.invariants.get(s) match {
        case Some(inv) => evalCondition(inv, potentialNextRx)
        case None => true
      }
    )
    if (invariantsHold) Some(applyTimeouts(potentialNextRx)) else None
  }

  def advanceTimeZone(rx0: RxGraph): Option[RxGraph] = {
    if (rx0.clocks.isEmpty && rx0.pendingDelays.isEmpty) return Some(rx0)
    val delayedZone = rx0.zone.delay
    var currentZone = Option(delayedZone)
    for (st <- rx0.inits) {
      rx0.invariants.get(st).foreach { inv =>
        currentZone = currentZone.flatMap(z => intersectConditionWithZone(inv, z, rx0))
      }
    }
    for (pending <- rx0.pendingDelays) {
      val clock = pending._3
      val targetVal = pending._4
      currentZone = currentZone.flatMap(z =>
        if (z.clocks.contains(clock)) z.constrain(clock, DBM.ZERO_CLOCK, targetVal, isStrict = false) else Some(z))
    }
    currentZone.map(z => rx0.copy(zone = z.extrapolate(effectiveMax(rx0))))
  }

  def intersectConditionWithZone(cond: Condition, zone: DBM.Zone, rx: RxGraph): Option[DBM.Zone] = cond match {
    case Condition.AtomicCond(UpdateExpr.Var(clock), op, rhs) if rx.clocks.contains(clock) => 
      val v = Condition.extractDouble(evalExpr(rhs, rx.val_env, rx))
      op match {
        case "<"  => zone.constrain(clock, DBM.ZERO_CLOCK, v, isStrict = true)
        case "<=" => zone.constrain(clock, DBM.ZERO_CLOCK, v, isStrict = false)
        case ">"  => zone.constrain(DBM.ZERO_CLOCK, clock, -v, isStrict = true)
        case ">=" => zone.constrain(DBM.ZERO_CLOCK, clock, -v, isStrict = false)
        case "==" | "=" => 
          zone.constrain(clock, DBM.ZERO_CLOCK, v, isStrict = false)
              .flatMap(_.constrain(DBM.ZERO_CLOCK, clock, -v, isStrict = false))
        case _    => Some(zone)
      }
      
    case Condition.AtomicCond(lhs, op, UpdateExpr.Var(clock)) if rx.clocks.contains(clock) =>
      val invOp = op match { case "<" => ">"; case "<=" => ">="; case ">" => "<"; case ">=" => "<="; case _ => op }
      intersectConditionWithZone(Condition.AtomicCond(UpdateExpr.Var(clock), invOp, lhs), zone, rx)
      
    case Condition.And(l, r) =>
      intersectConditionWithZone(l, zone, rx).flatMap(z => intersectConditionWithZone(r, z, rx))
      
    case Condition.Or(l, r) =>
      intersectConditionWithZone(l, zone, rx).orElse(intersectConditionWithZone(r, zone, rx))
      
    case _ => Some(zone)
  }

  def intersectAll(cond: Condition, zone: DBM.Zone, rx: RxGraph): List[DBM.Zone] = cond match {
    case Condition.AtomicCond(UpdateExpr.Var(clock), op, rhs) if rx.clocks.contains(clock) =>
      if (op == "!=") {
        val v = Condition.extractDouble(evalExpr(rhs, rx.val_env, rx))
        (zone.constrain(clock, DBM.ZERO_CLOCK, v, isStrict = true).toList ++
          zone.constrain(DBM.ZERO_CLOCK, clock, -v, isStrict = true).toList).distinct
      } else intersectConditionWithZone(cond, zone, rx).toList
    case Condition.AtomicCond(lhs, op, UpdateExpr.Var(clock)) if rx.clocks.contains(clock) =>
      val invOp = op match { case "<" => ">"; case "<=" => ">="; case ">" => "<"; case ">=" => "<="; case _ => op }
      intersectAll(Condition.AtomicCond(UpdateExpr.Var(clock), invOp, lhs), zone, rx)
    case Condition.And(l, r) =>
      intersectAll(l, zone, rx).flatMap(z => intersectAll(r, z, rx)).distinct
    case Condition.Or(l, r) =>
      (intersectAll(l, zone, rx) ++ intersectAll(r, zone, rx)).distinct
    case other =>
      if (evalCondition(other, rx)) List(zone) else Nil
  }

  def next[Name >: QName](rx: RxGraph): Set[(Name, RxGraph)] =
    nextEdge(rx).map(e => e._1._4 -> e._2) ++ nextDelay(rx)
    
  given Conversion[RxGraph, RxGraph] = identity
}