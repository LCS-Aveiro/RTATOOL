package rta.backend

import rta.syntax.Program2.RxGraph
import rta.syntax.LtlFormula
import rta.syntax.LtlFormula.*
import scala.util.boundary, boundary.break

object LtlEvaluator {

  sealed trait Mode
  case object ReTA   extends Mode
  case object ReGA   extends Mode
  case object Hybrid extends Mode

  def eval(trace: List[RxGraph], phi: LtlFormula, mode: Mode = Hybrid, isSymbolic: Boolean = false): Boolean = {
    if (trace.isEmpty) return false
    val tr = trace.toVector
    satAt(tr, phi, mode, isSymbolic)(0)
  }

  private def leaf(g: RxGraph, f: LtlFormula, mode: Mode, isSymbolic: Boolean): Boolean = (f, mode) match {
    case (StateProp(_), ReGA) => throw new Exception("ReGA usa semântica de variáveis, não arestas.")
    case (CondProp(_), ReTA)  => throw new Exception("ReTA usa semântica de aresta, não variáveis.")
    case (StateProp(e), _)    => g.act.exists(_._4 == e)
    case (CondProp(c), _)     => 
      if (isSymbolic) RxSemantics.evalConditionForLTL(c, g) 
      else RxSemantics.evalCondition(c, g)
    case _                    => sys.error("leaf chamado com fórmula não-atómica")
  }

  private def satAt(tr: Vector[RxGraph], f: LtlFormula, mode: Mode, isSymbolic: Boolean): Array[Boolean] = {
    val n = tr.size
    f match {
      case True  => Array.fill(n)(true)
      case False => Array.fill(n)(false)
      case StateProp(_) | CondProp(_) => Array.tabulate(n)(i => leaf(tr(i), f, mode, isSymbolic))

      case Not(p)     => satAt(tr, p, mode, isSymbolic).map(!_)
      case And(p, q)  => (satAt(tr, p, mode, isSymbolic), satAt(tr, q, mode, isSymbolic)).zipped.map(_ && _).toArray
      case Or(p, q)   => (satAt(tr, p, mode, isSymbolic), satAt(tr, q, mode, isSymbolic)).zipped.map(_ || _).toArray
      case Impl(p, q) => (satAt(tr, p, mode, isSymbolic), satAt(tr, q, mode, isSymbolic)).zipped.map((a, b) => !a || b).toArray
      case Iff(p, q)  => (satAt(tr, p, mode, isSymbolic), satAt(tr, q, mode, isSymbolic)).zipped.map(_ == _).toArray

      case Next(p) =>
        val pOk = satAt(tr, p, mode, isSymbolic)
        Array.tabulate(n)(i => i + 1 < n && pOk(i + 1))

      case Globally(p) =>
        val pOk = satAt(tr, p, mode, isSymbolic)
        val res = new Array[Boolean](n)
        var i = n - 1
        while (i >= 0) { res(i) = pOk(i) && (i + 1 >= n || res(i + 1)); i -= 1 }
        res

      case Eventually(p) =>
        val pOk = satAt(tr, p, mode, isSymbolic)
        val res = new Array[Boolean](n)
        var i = n - 1
        while (i >= 0) { res(i) = pOk(i) || (i + 1 < n && res(i + 1)); i -= 1 }
        res

      case Until(p, q) =>
        val pOk = satAt(tr, p, mode, isSymbolic)
        val qOk = satAt(tr, q, mode, isSymbolic)
        val res = new Array[Boolean](n)
        var i = n - 1
        while (i >= 0) { res(i) = qOk(i) || (pOk(i) && i + 1 < n && res(i + 1)); i -= 1 }
        res
    }
  }

  def evalTimedUntil(
    trace: List[(RxGraph, Double)],
    phi: LtlFormula, psi: LtlFormula,
    lower: Double, upper: Double,
    mode: Mode = Hybrid,
    isSymbolic: Boolean = false
  ): Boolean = boundary {
    var elapsed = 0.0
    for ((config, delay) <- trace) {
      if (elapsed > upper) break(false)
      if (elapsed >= lower && leaf(config, psi, mode, isSymbolic)) break(true)
      if (!leaf(config, phi, mode, isSymbolic) && !leaf(config, psi, mode, isSymbolic)) break(false)
      elapsed += delay
    }
    false
  }
}