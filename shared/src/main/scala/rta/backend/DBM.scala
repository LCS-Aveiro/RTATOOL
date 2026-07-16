package rta.backend

import rta.syntax.Program2.QName

object DBM {
  val ZERO_CLOCK = QName(List("0"))
  val INF = Double.PositiveInfinity

  case class Bound(value: Double, strict: Boolean) {
    def +(other: Bound): Bound = Bound(value + other.value, strict || other.strict)
    
    def min(other: Bound): Bound = {
      if (this.value < other.value) this
      else if (this.value == other.value && this.strict && !other.strict) this
      else other
    }
  }

  val INF_BOUND = Bound(INF, false)

  case class Zone(clocks: Set[QName], matrix: Map[(QName, QName), Bound]) {
    
    // CORREÇÃO: Agora rejeita (0 < 0). Só aceita valores > 0, ou = 0 desde que não seja estrito (<=)
    def isSatisfiable: Boolean = clocks.forall { c => 
      val b = matrix((c, c))
      b.value > 0 || (b.value == 0 && !b.strict)
    }


    def delay: Zone = {
      var newMat = matrix
      for (x <- clocks if x != ZERO_CLOCK) {
        newMat = newMat.updated((x, ZERO_CLOCK), INF_BOUND)
      }
      Zone(clocks, newMat).canonicalize
    }

    def constrain(x: QName, y: QName, v: Double, isStrict: Boolean): Option[Zone] = {
      val newBound = Bound(v, isStrict)
      val currentBound = matrix((x, y))
      
      if (newBound.value < currentBound.value || (newBound.value == currentBound.value && newBound.strict && !currentBound.strict)) {
        val updated = Zone(clocks, matrix.updated((x, y), newBound)).canonicalize
        if (updated.isSatisfiable) Some(updated) else None
      } else {
        Some(this) 
      }
    }

    def reset(x: QName): Zone = {
      var newMat = matrix
      for (y <- clocks) {
        newMat = newMat.updated((x, y), matrix((ZERO_CLOCK, y)))
        newMat = newMat.updated((y, x), matrix((y, ZERO_CLOCK)))
      }
      Zone(clocks, newMat).canonicalize
    }


    def extrapolate(maxConst: Map[QName, Double]): Zone = {
      var newMat = matrix
      val nonZero = clocks - ZERO_CLOCK

      for (x <- nonZero) {
        val mx = maxConst.getOrElse(x, 0.0)
        val upperX    = newMat((x, ZERO_CLOCK)).value
        val lowerXNeg = newMat((ZERO_CLOCK, x)).value

        if (upperX > mx) {
          for (y <- clocks if y != x) newMat = newMat.updated((x, y), INF_BOUND)
        }
        if (-lowerXNeg > mx) {
          for (y <- clocks if y != x) newMat = newMat.updated((y, x), INF_BOUND)
        }
      }

      Zone(clocks, newMat).canonicalize
    }

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

  def initial(clocks: Set[QName]): Zone = {
    val allClocks = clocks + ZERO_CLOCK
    val matrix = (for (c1 <- allClocks; c2 <- allClocks) yield (c1, c2) -> Bound(0.0, strict = false)).toMap
    Zone(allClocks, matrix)
  }
}