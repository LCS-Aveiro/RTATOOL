package rta.backend

import rta.syntax.Program2.{ Edge, Edges, QName, RxGraph}
import rta.syntax.{Condition, CounterUpdate, Statement, UpdateExpr, UpdateStmt, IfThenStmt}
import rta.syntax.Program2.showEdge
import rta.syntax.Program2.showEdges
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

  def toOnOff(e: Edge, rx: RxGraph): (Edges, Edges, Map[QName, Int]) = {
    val (toA, toD, upds) = getHyperEdgeEffects(e, rx)
    val (nextEnv, _) = applyUpdates(upds, rx)
    (toA, toD, nextEnv)
  }


  private def getHyperEdgeEffects(e: Edge, rx: RxGraph): (Edges, Edges, List[Statement]) = {
    val triggeredHyperEdges = from(e, rx)
    var toActivate = Set.empty[Edge]
    var toDeactivate = Set.empty[Edge]
    var updatesToApply = List.empty[Statement]

    for (hyperEdge <- triggeredHyperEdges) {
      val (triggerLabel, targetLabel, ruleId, ruleLabel) = hyperEdge
      
      if (rx.act.contains(hyperEdge)) {
        
        val conditionHolds = rx.edgeConditions.getOrElse(hyperEdge, None) match {
          case Some(cond) => Condition.evaluate(cond, rx.val_env, rx.clock_env)
          case None => true
        }

        if (conditionHolds) {
          updatesToApply = updatesToApply ::: rx.edgeUpdates.getOrElse(hyperEdge, Nil)

          if (rx.on.getOrElse(triggerLabel, Set.empty).contains((targetLabel, ruleId, ruleLabel))) {
            toActivate = toActivate ++ rx.lbls.getOrElse(targetLabel, Set.empty)
          }
          
          if (rx.off.getOrElse(triggerLabel, Set.empty).contains((targetLabel, ruleId, ruleLabel))) {
            toDeactivate = toDeactivate ++ rx.lbls.getOrElse(targetLabel, Set.empty)
          }
        }
      }
    }
    (toActivate, toDeactivate, updatesToApply)
  }

  /**
   * Lógica de execução de updates e resets de relógios.
   */
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

    var currentValEnv = rx.val_env
    var clockResets = Map[QName, Double]()

    def processStatements(s_list: List[Statement]): Unit = {
      for (stmt <- s_list) {
        stmt match {
          case UpdateStmt(upd) =>
            if (rx.clocks.contains(upd.variable)) {
              upd.expr match {
                case UpdateExpr.Lit(0) => clockResets += (upd.variable -> 0.0)
                case _ =>
              }
            } else {
              currentValEnv += (upd.variable -> evaluateUpdateExpr(upd.expr, currentValEnv))
            }
          case IfThenStmt(condition, thenStmts) =>
            if (Condition.evaluate(condition, currentValEnv, rx.clock_env)) processStatements(thenStmts)
        }
      }
    }

    processStatements(stmts)
    (currentValEnv, clockResets)
  }

  private def checkInvariant(state: QName, rx: RxGraph): Boolean = {
    rx.invariants.get(state) match {
      case Some(inv) => Condition.evaluate(inv, rx.val_env, rx.clock_env)
      case None => true
    }
  }

  /**
   * Calcula as arestas que podem ser disparadas no estado atual.
   */
  def nextEdge(rx: RxGraph): Set[(Edge, RxGraph)] =
    (for
      st <- rx.inits
      (st2, tId, lbl) <- rx.edg.getOrElse(st, Set.empty)
      edge: Edge = (st, st2, tId, lbl)
      if rx.act.contains(edge)
      if rx.edgeConditions.getOrElse(edge, None).forall(c => Condition.evaluate(c, rx.val_env, rx.clock_env))
    yield
      val (toAct, toDeact, hyperStmts) = getHyperEdgeEffects(edge, rx)
      val allStatements = rx.edgeUpdates.getOrElse(edge, Nil) ++ hyperStmts

      val (finalValEnv, clockResets) = applyUpdates(allStatements, rx)
      val finalClockEnv = rx.clock_env.map { case (c, v) => c -> clockResets.getOrElse(c, v) }

      val newAct = (rx.act ++ toAct) -- toDeact
      val newInits = (rx.inits - st) + st2

      (edge, rx.copy(inits = newInits, act = newAct, val_env = finalValEnv, clock_env = finalClockEnv))
    ).filter { case (_, nextRx) =>
      nextRx.inits.forall(s => checkInvariant(s, nextRx))
    }

  def nextDelay(rx: RxGraph): Set[(QName, RxGraph)] = {
    if (rx.clocks.isEmpty) return Set.empty
    val delayedClockEnv = rx.clock_env.map { case (c, v) => (c, v + 0.000001) }
    val potentialNextRx = rx.copy(clock_env = delayedClockEnv)
    if (rx.inits.forall(s => checkInvariant(s, potentialNextRx))) 
      Set((QName(List("delay")), potentialNextRx))
    else Set.empty
  }

  def next[Name >: QName](rx: RxGraph): Set[(Name, RxGraph)] =
    nextEdge(rx).map(e => e._1._4 -> e._2) ++ nextDelay(rx)
}