package rta.backend

import rta.syntax.Program2.{Edge, Edges, QName, RxGraph}
import rta.syntax.{Condition, Statement, UpdateExpr, AssignStmt, ArrayAssignStmt, IfThenStmt, ForeachStmt, ReturnStmt, PrintStmt, RuntimeValue}
import scala.annotation.tailrec

object RxSemantics {

  private val EPSILON = 1e-7

  // Avaliação de Expressões (lê clocks como floats se a variável for um clock)
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
           rx.clock_env.get(globalName).map(RuntimeValue.VFloat(_))
             .orElse(env.get(globalName))
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
      rx.functions.get(funcName) match {
        case Some(funcDef) =>
          val evalArgs = args.map(a => evalExpr(a, env, rx))
          var localEnv = env
          funcDef.params.zip(evalArgs).foreach { case (param, v) => localEnv += (param -> v) }
          // Executa a função localmente (clocks não são alterados em scope de função puramente lógico)
          val (finalEnv, _) = applyUpdates(funcDef.body, rx.copy(val_env = localEnv))
          finalEnv.getOrElse(QName(List("__return")), RuntimeValue.VInt(0))
        case None => RuntimeValue.VInt(0)
      }
  }

  def evalCondition(cond: Condition, rx: RxGraph): Boolean = cond match {
    case Condition.AtomicCond(l, op, r) =>
      Condition.compareValues(evalExpr(l, rx.val_env, rx), op, evalExpr(r, rx.val_env, rx))
    case Condition.And(l, r) => evalCondition(l, rx) && evalCondition(r, rx)
    case Condition.Or(l, r) => evalCondition(l, rx) || evalCondition(r, rx)
  }

  def applyUpdates(stmts: List[Statement], rx: RxGraph): (Map[QName, RuntimeValue], Map[QName, Double]) = {
    var currentEnv = rx.val_env
    var currentClockEnv = rx.clock_env
    val returnKey = QName(List("__return"))

    def assignWithBounds(q: QName, newVal: RuntimeValue): Unit = {
      val existing = currentEnv.get(q)
      existing match {
        case Some(RuntimeValue.VInt(_, minOpt, maxOpt)) =>
          val v = newVal match {
            case RuntimeValue.VInt(i, _, _) => i
            case RuntimeValue.VFloat(f, _, _) => f.toInt
            case _ => 0
          }
          val cappedMin = minOpt.map(m => Math.max(m, v)).getOrElse(v)
          val finalVal = maxOpt.map(m => Math.min(m, cappedMin)).getOrElse(cappedMin)
          currentEnv += (q -> RuntimeValue.VInt(finalVal, minOpt, maxOpt))
        case Some(RuntimeValue.VFloat(_, minOpt, maxOpt)) =>
          val v = newVal match {
            case RuntimeValue.VFloat(f, _, _) => f
            case RuntimeValue.VInt(i, _, _) => i.toDouble
            case _ => 0.0
          }
          val cappedMin = minOpt.map(m => Math.max(m, v)).getOrElse(v)
          val finalVal = maxOpt.map(m => Math.min(m, cappedMin)).getOrElse(cappedMin)
          currentEnv += (q -> RuntimeValue.VFloat(finalVal, minOpt, maxOpt))
        case _ => currentEnv += (q -> newVal)
      }
    }

    def process(ss: List[Statement]): Unit = {
      val it = ss.iterator
      while (it.hasNext && !currentEnv.contains(returnKey)) {
        it.next() match {
          case AssignStmt(v, expr) =>
            val evaluated = evalExpr(expr, currentEnv, rx.copy(clock_env = currentClockEnv))
            if (rx.clocks.contains(v)) {
               currentClockEnv += (v -> Condition.extractDouble(evaluated))
            } else {
               assignWithBounds(v, evaluated)
            }

          case ArrayAssignStmt(arrName, idxExpr, valExpr) =>
            val idx = Condition.extractDouble(evalExpr(idxExpr, currentEnv, rx.copy(clock_env = currentClockEnv))).toInt
            val value = evalExpr(valExpr, currentEnv, rx.copy(clock_env = currentClockEnv))
            currentEnv.get(arrName) match {
              case Some(RuntimeValue.VArray(elems, isDyn, maxOpt)) =>
                if (idx >= 0 && idx < elems.length) {
                  currentEnv += (arrName -> RuntimeValue.VArray(elems.updated(idx, value), isDyn, maxOpt))
                } else if (isDyn && idx == elems.length) {
                  val newElems = elems :+ value
                  currentEnv += (arrName -> RuntimeValue.VArray(maxOpt.map(m => newElems.takeRight(m)).getOrElse(newElems), isDyn, maxOpt))
                }
              case _ =>
            }

          case IfThenStmt(cond, thens) =>
            if (evalCondition(cond, rx.copy(val_env = currentEnv, clock_env = currentClockEnv))) process(thens)

          case ForeachStmt(iter, arr, body) =>
            currentEnv.get(arr) match {
              case Some(RuntimeValue.VArray(elems, _, _)) =>
                val eIt = elems.iterator
                while(eIt.hasNext && !currentEnv.contains(returnKey)) {
                  currentEnv += (iter -> eIt.next())
                  process(body)
                }
                currentEnv -= iter
              case _ =>
            }

          case ReturnStmt(expr) =>
            currentEnv += (returnKey -> evalExpr(expr, currentEnv, rx.copy(clock_env = currentClockEnv)))

          case PrintStmt(expr) =>
            val evaluated = evalExpr(expr, currentEnv, rx.copy(clock_env = currentClockEnv))
            println(s"🖨️ RTA Print | ${UpdateExpr.show(expr)} = ${evaluated.value}")
        }
      }
    }

    process(stmts)
    (currentEnv, currentClockEnv)
  }

  def from(e: Edge, rx: RxGraph): Set[Edge] = cascade(Set(e._4), Set())(using rx)

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

  private def getHyperEdgeEffects(e: Edge, rx: RxGraph): (Edges, Edges, List[Statement], Set[(Edge, String, QName, Double)]) = {
    val triggeredHyperEdges = from(e, rx)
    var toActivate = Set.empty[Edge]
    var toDeactivate = Set.empty[Edge]
    var updatesToApply = List.empty[Statement]
    var newPending = Set.empty[(Edge, String, QName, Double)]

    for (hyperEdge <- triggeredHyperEdges) {
      if (rx.act.contains(hyperEdge)) {
        val conditionHolds = rx.edgeConditions.getOrElse(hyperEdge, None).forall(c => evalCondition(c, rx))
        
        if (conditionHolds) {
          updatesToApply = updatesToApply ::: rx.edgeUpdates.getOrElse(hyperEdge, Nil)
          val (triggerLabel, targetLabel, ruleId, ruleLabel) = hyperEdge
          
          val isOn = rx.on.getOrElse(triggerLabel, Set.empty).contains((targetLabel, ruleId, ruleLabel))
          val isOff = rx.off.getOrElse(triggerLabel, Set.empty).contains((targetLabel, ruleId, ruleLabel))
          
          if (rx.delays.contains(ruleLabel)) {
            val (clock, delayVal) = rx.delays(ruleLabel)
            val currentClock = rx.clock_env.getOrElse(clock, 0.0)
            if (isOn) newPending += ((hyperEdge, "on", clock, currentClock + delayVal))
            if (isOff) newPending += ((hyperEdge, "off", clock, currentClock + delayVal))
          } else {
            if (isOn) toActivate = toActivate ++ rx.lbls.getOrElse(targetLabel, Set.empty)
            if (isOff) toDeactivate = toDeactivate ++ rx.lbls.getOrElse(targetLabel, Set.empty)
          }
        }
      }
    }
    (toActivate, toDeactivate, updatesToApply, newPending)
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

  def nextEdge(rx0: RxGraph): Set[(Edge, RxGraph)] = {
    val rx = applyTimeouts(rx0)
    val transitions = for {
      st <- rx.inits
      (st2, tid, lbl) <- rx.edg.getOrElse(st, Set.empty)
      edge = (st, st2, tid, lbl)
      if rx.act.contains(edge)
      if rx.edgeConditions.getOrElse(edge, None).forall(c => evalCondition(c, rx))
    } yield {
      val (toAct, toDeact, hStmts, newPending) = getHyperEdgeEffects(edge, rx)
      val currentAct = (rx.act ++ toAct) -- toDeact
      
      val allStmts = rx.edgeUpdates.getOrElse(edge, Nil) ++ hStmts
      val (nextEnv, nextClockEnv) = applyUpdates(allStmts, rx)
      
      (edge, rx.copy(
        inits = (rx.inits - st) + st2,
        act = currentAct,
        val_env = nextEnv,
        clock_env = nextClockEnv,
        pendingDelays = rx.pendingDelays ++ newPending
      ))
    }

    transitions.filter { case (_, nextRx) =>
      nextRx.inits.forall(s => checkInvariant(s, nextRx))
    }
  }

  def nextDelay(rx0: RxGraph): Set[(QName, RxGraph)] = {
    val rx = applyTimeouts(rx0)
    if (rx.clocks.isEmpty) return Set.empty
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
       
       val nextRx = rx.copy(
         act = (rx.act ++ toAct) -- toDeact, 
         pendingDelays = rx.pendingDelays -- matured
       )
       applyTimeouts(nextRx)
    } else rx
  }

  def next[Name >: QName](rx: RxGraph): Set[(Name, RxGraph)] =
    nextEdge(rx).map(e => e._1._4 -> e._2) ++ nextDelay(rx)
}