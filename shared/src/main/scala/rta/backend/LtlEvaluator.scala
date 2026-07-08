package rta.backend

import rta.syntax.Program2.RxGraph
import rta.syntax.LtlFormula
import rta.syntax.LtlFormula.*
import scala.util.boundary, boundary.break

object LtlEvaluator {

  sealed trait Mode
  case object ReTA   extends Mode // só StateProp (arestas); CondProp é erro
  case object ReGA   extends Mode // só CondProp (variáveis); StateProp é erro
  case object Hybrid extends Mode // aceita ambos (frontend do utilizador)

  /** Avalia phi sobre o traço, no ponto 0. O(n) em vez de O(n²). */
  def eval(trace: List[RxGraph], phi: LtlFormula, mode: Mode = Hybrid): Boolean = {
    if (trace.isEmpty) return false
    val tr = trace.toVector
    satAt(tr, phi, mode)(0)
  }

  private def leaf(g: RxGraph, f: LtlFormula, mode: Mode): Boolean = (f, mode) match {
    case (StateProp(_), ReGA) => throw new Exception("ReGA usa semântica de variáveis, não arestas.")
    case (CondProp(_), ReTA)  => throw new Exception("ReTA usa semântica de aresta, não variáveis.")
    case (StateProp(e), _)    => g.act.exists(_._4 == e)
    case (CondProp(c), _)     => RxSemantics.evalCondition(c, g)
    case _                    => sys.error("leaf chamado com fórmula não-atómica")
  }

  // Calcula, para toda a posição i do traço, se σ[i...] |= f. Memoização
  // implícita: cada subfórmula é resolvida uma única vez, de trás para a
  // frente, e os operadores temporais reaproveitam o resultado da posição
  // seguinte em vez de reavaliar tudo (era esse o custo quadrático antigo).
  private def satAt(tr: Vector[RxGraph], f: LtlFormula, mode: Mode): Array[Boolean] = {
    val n = tr.size
    f match {
      case True  => Array.fill(n)(true)
      case False => Array.fill(n)(false)
      case StateProp(_) | CondProp(_) => Array.tabulate(n)(i => leaf(tr(i), f, mode))

      case Not(p)     => satAt(tr, p, mode).map(!_)
      case And(p, q)  => (satAt(tr, p, mode), satAt(tr, q, mode)).zipped.map(_ && _).toArray
      case Or(p, q)   => (satAt(tr, p, mode), satAt(tr, q, mode)).zipped.map(_ || _).toArray
      case Impl(p, q) => (satAt(tr, p, mode), satAt(tr, q, mode)).zipped.map((a, b) => !a || b).toArray
      case Iff(p, q)  => (satAt(tr, p, mode), satAt(tr, q, mode)).zipped.map(_ == _).toArray

      case Next(p) =>
        val pOk = satAt(tr, p, mode)
        Array.tabulate(n)(i => i + 1 < n && pOk(i + 1))

      case Globally(p) =>
        val pOk = satAt(tr, p, mode)
        val res = new Array[Boolean](n)
        var i = n - 1
        while (i >= 0) { res(i) = pOk(i) && (i + 1 >= n || res(i + 1)); i -= 1 }
        res

      case Eventually(p) =>
        val pOk = satAt(tr, p, mode)
        val res = new Array[Boolean](n)
        var i = n - 1
        while (i >= 0) { res(i) = pOk(i) || (i + 1 < n && res(i + 1)); i -= 1 }
        res

      case Until(p, q) =>
        val pOk = satAt(tr, p, mode)
        val qOk = satAt(tr, q, mode)
        val res = new Array[Boolean](n)
        var i = n - 1
        while (i >= 0) { res(i) = qOk(i) || (pOk(i) && i + 1 < n && res(i + 1)); i -= 1 }
        res
    }
  }

  // -----------------------------------------------------------------------
  // TCTL: Until temporizado sobre um traço com atrasos reais entre estados.
  // Aqui o `boundary` já compensa a sério: em vez de percorrer sempre o
  // traço inteiro (como no DP acima, que é sobre índices discretos), temos
  // de parar assim que o tempo acumulado ultrapassa o limite superior do
  // intervalo J = [lower, upper] — continuar depois disso é trabalho morto.
  // Corresponde à Def. 18 do artigo (Φ U_J Ψ).
  // -----------------------------------------------------------------------
  def evalTimedUntil(
    trace: List[(RxGraph, Double)], // (configuração, delay até ao próximo passo)
    phi: LtlFormula, psi: LtlFormula,
    lower: Double, upper: Double,
    mode: Mode = Hybrid
  ): Boolean = boundary {
    var elapsed = 0.0
    for ((config, delay) <- trace) {
      if (elapsed > upper) break(false) // já saiu do intervalo, não vale mais a pena continuar
      if (elapsed >= lower && leaf(config, psi, mode)) break(true)
      if (!leaf(config, phi, mode) && !leaf(config, psi, mode)) break(false) // nem phi nem psi seguram
      elapsed += delay
    }
    false
  }
}