package rta.backend

import rta.syntax.Program2.{ Edge, Edges, QName, RxGraph}
import rta.syntax.{Condition, CounterUpdate, Statement, UpdateExpr, UpdateStmt, IfThenStmt}
import scala.annotation.tailrec

object RxSemantics {

  def from(e: Edge, rx: RxGraph): Set[Edge] =
    cascade(Set(e._4), Set())(using rx)

  @tailrec
  private def cascade(pendingLabels: Set[QName], doneEdges: Set[Edge])(using rx: RxGraph): Edges = {
    if (pendingLabels.isEmpty) doneEdges
    else {
      val currentLabel = pendingLabels.head
      val remainingLabels = pendingLabels.tail
      val rulesOn = rx.on.getOrElse(currentLabel, Set.empty).map(t => (currentLabel, t._1, t._2, t._3))
      val rulesOff = rx.off.getOrElse(currentLabel, Set.empty).map(t => (currentLabel, t._1, t._2, t._3))
      val allNewRules = (rulesOn ++ rulesOff).filter(rx.act.contains) -- doneEdges
      val newLabels = allNewRules.map(_._4).filter(_.n.nonEmpty)
      cascade(remainingLabels ++ newLabels, doneEdges ++ allNewRules)
    }
  }

  def toOnOff(e: Edge, rx0: RxGraph): (Edges, Edges, Map[QName, Int]) = {
    val rx = applyTimeouts(rx0)
    val (toA, toD, upds, _) = getHyperEdgeEffects(e, rx)
    // Aqui usamos o snapshot para o val_env reativo
    val (nextEnv, _) = applyUpdates(upds, rx)
    (toA, toD, nextEnv)
  }

  private def getHyperEdgeEffects(e: Edge, rx: RxGraph): (Edges, Edges, List[Statement], Set[(Edge, String, QName, Double)]) = {
    val triggeredHyperEdges = from(e, rx)
    var toActivate = Set.empty[Edge]
    var toDeactivate = Set.empty[Edge]
    var updatesToApply = List.empty[Statement]
    var newPending = Set.empty[(Edge, String, QName, Double)]

    for (hyperEdge <- triggeredHyperEdges) {
      if (rx.act.contains(hyperEdge)) {
        // IMPORTANTE: Avalia a condição da regra usando os valores atuais do grafo
        val conditionHolds = rx.edgeConditions.getOrElse(hyperEdge, None) match {
          case Some(cond) => Condition.evaluate(cond, rx.val_env, rx.clock_env)
          case None => true
        }
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

  // --- AQUI ESTÁ A SUA LÓGICA DE SNAPSHOT ADAPTADA ---
  def applyUpdates(stmts: List[Statement], rx: RxGraph): (Map[QName, Int], Map[QName, Double]) = {
    def evaluateUpdateExpr(expr: UpdateExpr, env: Map[QName, Int]): Int = {
      expr match {
        case UpdateExpr.Lit(i) => i
        case UpdateExpr.Var(q) => env.getOrElse(q, 0)
        case UpdateExpr.Add(v, e) =>
          val vVal = env.getOrElse(v, 0)
          val eVal = e match { case Left(i) => i; case Right(q) => env.getOrElse(q, 0) }
          vVal + eVal
        case UpdateExpr.Sub(v, e) =>
          val vVal = env.getOrElse(v, 0)
          val eVal = e match { case Left(i) => i; case Right(q) => env.getOrElse(q, 0) }
          vVal - eVal
      }
    }

    // Snapshots: Valores originais antes de qualquer mudança
    val originalValEnv = rx.val_env
    val originalClockEnv = rx.clock_env
    
    var nextValUpdates = Map[QName, Int]()
    var nextClockResets = Map[QName, Double]()

    def processStatements(s_list: List[Statement]): Unit = {
      for (stmt <- s_list) {
        stmt match {
          case UpdateStmt(upd) =>
            if (rx.clocks.contains(upd.variable)) {
              // Reset de Relógio: x' := 0
              upd.expr match {
                case UpdateExpr.Lit(0) => nextClockResets += (upd.variable -> 0.0)
                case _ => // Outros valores ignorados em relógios simples
              }
            } else {
              // Variável Inteira: t' := c (c é lido do snapshot)
              val newValue = evaluateUpdateExpr(upd.expr, originalValEnv)
              nextValUpdates += (upd.variable -> newValue)
            }
          case IfThenStmt(condition, thenStmts) =>
            // Condição avaliada no snapshot
            if (Condition.evaluate(condition, originalValEnv, originalClockEnv)) {
              processStatements(thenStmts)
            }
        }
      }
    }

    processStatements(stmts)
    
    // Aplica os updates sobre os valores originais
    (originalValEnv ++ nextValUpdates, originalClockEnv ++ nextClockResets)
  }

  private def checkInvariant(state: QName, rx: RxGraph): Boolean = {
    rx.invariants.get(state) match {
      case Some(inv) => Condition.evaluate(inv, rx.val_env, rx.clock_env)
      case None => true
    }
  }

  def nextEdge(rx0: RxGraph): Set[(Edge, RxGraph)] = {
    val rx = applyTimeouts(rx0)
    (for
      st <- rx.inits
      (st2, tId, lbl) <- rx.edg.getOrElse(st, Set.empty)
      edge: Edge = (st, st2, tId, lbl)
      if rx.act.contains(edge)
      // Condição da aresta no snapshot inicial
      if rx.edgeConditions.getOrElse(edge, None).forall(c => Condition.evaluate(c, rx.val_env, rx.clock_env))
    yield
      val (toAct, toDeact, hyperStmts, newPending) = getHyperEdgeEffects(edge, rx)
      val allStatements = rx.edgeUpdates.getOrElse(edge, Nil) ++ hyperStmts
      // Aplica a lógica de snapshot para obter os novos ambientes
      val (finalValEnv, finalClockEnv) = applyUpdates(allStatements, rx)
      val newAct = (rx.act ++ toAct) -- toDeact
      val newInits = (rx.inits - st) + st2
      (edge, rx.copy(inits = newInits, act = newAct, val_env = finalValEnv, clock_env = finalClockEnv, pendingDelays = rx.pendingDelays ++ newPending))
    ).filter { case (_, nextRx) =>
      // Verifica se o novo estado respeita os invariantes com os novos valores
      nextRx.inits.forall(s => checkInvariant(s, nextRx))
    }
  }

  def nextDelay(rx0: RxGraph): Set[(QName, RxGraph)] = {
    val rx = applyTimeouts(rx0)
    if (rx.clocks.isEmpty) return Set.empty
    val step = 0.000001 // Pequeno incremento para verificar se o tempo pode passar
        
    val canPass = rx.pendingDelays.forall { case (_, _, clock, targetVal) => 
        rx.clock_env.getOrElse(clock, 0.0) + step <= targetVal + 1e-9
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
       rx.clock_env.getOrElse(clock, 0.0) >= targetVal - 1e-9
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