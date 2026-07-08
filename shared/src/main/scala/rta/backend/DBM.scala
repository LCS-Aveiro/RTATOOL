package rta.backend

import rta.syntax.Program2.QName

object DBM {
  val ZERO_CLOCK = QName(List("0"))
  val INF = Double.PositiveInfinity

  // Define um limite: ex. "< 5" (strict = true) ou "<= 5" (strict = false)
  case class Bound(value: Double, strict: Boolean) {
    def +(other: Bound): Bound = Bound(value + other.value, strict || other.strict)
    
    // Serve para escolher o limite mais restritivo entre dois limites
    def min(other: Bound): Bound = {
      if (this.value < other.value) this
      else if (this.value == other.value && this.strict && !other.strict) this
      else other
    }
  }

  val INF_BOUND = Bound(INF, false)

  // Representa a "Zona" temporal atual
  case class Zone(clocks: Set[QName], matrix: Map[(QName, QName), Bound]) {
    
    // Verifica se a zona é matematicamente possível (não tem ciclos negativos)
    def isSatisfiable: Boolean = clocks.forall(c => matrix((c, c)).value >= 0)

    // 1. ADVANCE TIME: Deixa o tempo passar (Up Operation)
    // O tempo afeta todos os relógios igualmente, por isso a diferença entre eles mantém-se.
    // A única coisa que muda é que o limite superior deles face a ZERO passa a infinito.
    def delay: Zone = {
      var newMat = matrix
      for (x <- clocks if x != ZERO_CLOCK) {
        newMat = newMat.updated((x, ZERO_CLOCK), INF_BOUND)
      }
      Zone(clocks, newMat).canonicalize
    }

    // 2. CONSTRAIN: Avalia guardas e invariantes (ex: t <= 10)
    def constrain(x: QName, y: QName, v: Double, isStrict: Boolean): Option[Zone] = {
      val newBound = Bound(v, isStrict)
      val currentBound = matrix((x, y))
      
      // Se a nova restrição for mais apertada, atualiza e canonicaliza
      if (newBound.value < currentBound.value || (newBound.value == currentBound.value && newBound.strict && !currentBound.strict)) {
        val updated = Zone(clocks, matrix.updated((x, y), newBound)).canonicalize
        if (updated.isSatisfiable) Some(updated) else None
      } else {
        Some(this) // A restrição já estava garantida
      }
    }

    // 3. RESET: Coloca o relógio x a 0 (quando uma transição tem x' := 0)
    def reset(x: QName): Zone = {
      var newMat = matrix
      for (y <- clocks) {
        newMat = newMat.updated((x, y), matrix((ZERO_CLOCK, y)))
        newMat = newMat.updated((y, x), matrix((y, ZERO_CLOCK)))
      }
      Zone(clocks, newMat).canonicalize
    }

    // Algoritmo de Floyd-Warshall (garante que todas as restrições indiretas ficam explícitas)
    private def canonicalize: Zone = {
      var mat = matrix
      for (k <- clocks; i <- clocks; j <- clocks) {
        val direct = mat((i, j))
        val indirect = mat((i, k)) + mat((k, j))
        mat = mat.updated((i, j), direct.min(indirect))
      }
      Zone(clocks, mat)
    }
  }

  // Cria a Zona Inicial onde todos os relógios valem exatamente 0
  def initial(clocks: Set[QName]): Zone = {
    val allClocks = clocks + ZERO_CLOCK
    val matrix = (for (c1 <- allClocks; c2 <- allClocks) yield (c1, c2) -> Bound(0.0, strict = false)).toMap
    Zone(allClocks, matrix)
  }
}