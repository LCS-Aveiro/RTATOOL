package rta.backend

import rta.syntax.Program2.QName

object DBM {
  val ZERO_CLOCK = QName(List("0"))
  val INF = Double.PositiveInfinity

  case class Bound(value: Double, strict: Boolean) {
    def +(other: Bound): Bound =
      if (value == INF || other.value == INF) INF_BOUND
      else Bound(value + other.value, strict || other.strict)

    def min(other: Bound): Bound = {
      if (this.value < other.value) this
      else if (this.value == other.value && this.strict && !other.strict) this
      else other
    }
  }

  val INF_BOUND = Bound(INF, false)
  private val ZERO_BOUND = Bound(0.0, strict = false)

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
        var mat = matrix.updated((x, y), newBound)
        for (i <- clocks) {
          val ix = if (i == x) ZERO_BOUND else matrix((i, x))
          if (ix.value != INF) {
            val head = ix + newBound
            for (j <- clocks) {
              val yj = if (j == y) ZERO_BOUND else matrix((y, j))
              if (yj.value != INF) {
                val candidate = head + yj
                val current = mat((i, j))
                if (candidate.min(current) != current) mat = mat.updated((i, j), candidate)
              }
            }
          }
        }
        val updated = Zone(clocks, mat)
        if (updated.isSatisfiable) Some(updated) else None
      } else {
        Some(this)
      }
    }

    def reset(x: QName): Zone = assign(x, 0.0)

    def assign(x: QName, v: Double): Zone = {
      var newMat = matrix
      for (y <- clocks if y != x) {
        val up = matrix((ZERO_CLOCK, y))
        val lo = matrix((y, ZERO_CLOCK))
        newMat = newMat.updated((x, y), if (up.value == INF) up else Bound(up.value + v, up.strict))
        newMat = newMat.updated((y, x), if (lo.value == INF) lo else Bound(lo.value - v, lo.strict))
      }
      newMat = newMat.updated((x, x), ZERO_BOUND)
      Zone(clocks, newMat).canonicalize
    }

    def withClock(x: QName): Zone =
      if (clocks.contains(x)) reset(x)
      else {
        var newMat = matrix
        for (y <- clocks) {
          newMat = newMat.updated((x, y), matrix((ZERO_CLOCK, y)))
          newMat = newMat.updated((y, x), matrix((y, ZERO_CLOCK)))
        }
        newMat = newMat.updated((x, x), ZERO_BOUND)
        Zone(clocks + x, newMat)
      }

    def withoutClock(x: QName): Zone =
      if (!clocks.contains(x) || x == ZERO_CLOCK) this
      else Zone(clocks - x, matrix.filter { case ((a, b), _) => a != x && b != x })

    def extrapolate(maxConst: Map[QName, Double]): Zone = {
      def k(c: QName): Double = if (c == ZERO_CLOCK) 0.0 else maxConst.getOrElse(c, 0.0)
      var newMat = matrix
      var changed = false
      for (i <- clocks; j <- clocks if i != j) {
        val b = matrix((i, j))
        if (i != ZERO_CLOCK && b.value != INF && b.value > k(i)) {
          newMat = newMat.updated((i, j), INF_BOUND); changed = true
        } else if (j != ZERO_CLOCK && b.value < -k(j)) {
          newMat = newMat.updated((i, j), Bound(-k(j), strict = true)); changed = true
        }
      }
      if (changed) Zone(clocks, newMat).canonicalize else this
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
