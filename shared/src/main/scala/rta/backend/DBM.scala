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

  private def norm(v: Double): Double = v + 0.0

  private def tighter(v1: Double, s1: Boolean, v2: Double, s2: Boolean): Boolean =
    v1 < v2 || (v1 == v2 && s1 && !s2)

  private def orderClocks(cs: Iterable[QName]): Array[QName] =
    (ZERO_CLOCK +: (cs.toSet - ZERO_CLOCK).toArray.sortBy(_.toString)).toArray

  final class Matrix private[DBM] (z: Zone) {
    def apply(key: (QName, QName)): Bound = get(key).getOrElse(throw new NoSuchElementException(s"key not found: $key"))
    def get(key: (QName, QName)): Option[Bound] = {
      val i = z.indexOf(key._1)
      val j = z.indexOf(key._2)
      if (i < 0 || j < 0) None else Some(Bound(z.vals(i * z.n + j), z.strs(i * z.n + j)))
    }
  }

  final class Zone private[DBM] (private[DBM] val names: Array[QName], private[DBM] val vals: Array[Double], private[DBM] val strs: Array[Boolean]) {
    private[DBM] val n: Int = names.length

    lazy val clocks: Set[QName] = names.toSet

    def matrix: Matrix = new Matrix(this)

    private[DBM] def indexOf(q: QName): Int = {
      var i = 0
      while (i < n) { if (names(i) == q) return i; i += 1 }
      -1
    }

    private lazy val hash: Int =
      java.util.Arrays.hashCode(names.asInstanceOf[Array[AnyRef]]) * 31 * 31 +
        java.util.Arrays.hashCode(vals) * 31 + java.util.Arrays.hashCode(strs)

    override def hashCode: Int = hash

    override def equals(other: Any): Boolean = other match {
      case z: Zone =>
        (this eq z) || (hash == z.hash &&
          java.util.Arrays.equals(names.asInstanceOf[Array[AnyRef]], z.names.asInstanceOf[Array[AnyRef]]) &&
          java.util.Arrays.equals(vals, z.vals) && java.util.Arrays.equals(strs, z.strs))
      case _ => false
    }

    override def toString: String =
      (for (i <- 0 until n; j <- 0 until n if i != j && vals(i * n + j) != INF)
        yield s"${names(i)}-${names(j)}${if (strs(i * n + j)) "<" else "<="}${vals(i * n + j)}").mkString("Zone(", ", ", ")")

    // CORREÇÃO: Agora rejeita (0 < 0). Só aceita valores > 0, ou = 0 desde que não seja estrito (<=)
    def isSatisfiable: Boolean = DBM.satisfiable(n, vals, strs)

    def delay: Zone = {
      val v = vals.clone(); val s = strs.clone()
      var i = 1
      while (i < n) { v(i * n) = INF; s(i * n) = false; i += 1 }
      new Zone(names, v, s)
    }

    def constrain(x: QName, y: QName, value: Double, isStrict: Boolean): Option[Zone] = {
      val xi = indexOf(x); val yi = indexOf(y)
      val bv = norm(value)
      if (!tighter(bv, isStrict, vals(xi * n + yi), strs(xi * n + yi))) return Some(this)
      val v = vals.clone(); val s = strs.clone()
      v(xi * n + yi) = bv; s(xi * n + yi) = isStrict
      var i = 0
      while (i < n) {
        val ixv = if (i == xi) 0.0 else vals(i * n + xi)
        val ixs = if (i == xi) false else strs(i * n + xi)
        if (ixv != INF) {
          val hv = ixv + bv; val hs = ixs || isStrict
          var j = 0
          while (j < n) {
            val yjv = if (j == yi) 0.0 else vals(yi * n + j)
            val yjs = if (j == yi) false else strs(yi * n + j)
            if (yjv != INF) {
              val cv = norm(hv + yjv); val cs = hs || yjs
              if (tighter(cv, cs, v(i * n + j), s(i * n + j))) { v(i * n + j) = cv; s(i * n + j) = cs }
            }
            j += 1
          }
        }
        i += 1
      }
      if (DBM.satisfiable(n, v, s)) Some(new Zone(names, v, s)) else None
    }

    def reset(x: QName): Zone = assign(x, 0.0)

    def assign(x: QName, value: Double): Zone = {
      val xi = indexOf(x)
      val v = vals.clone(); val s = strs.clone()
      var y = 0
      while (y < n) {
        if (y != xi) {
          val up = vals(y); val ups = strs(y)
          val lo = vals(y * n); val los = strs(y * n)
          v(xi * n + y) = if (up == INF) INF else norm(up + value); s(xi * n + y) = if (up == INF) false else ups
          v(y * n + xi) = if (lo == INF) INF else norm(lo - value); s(y * n + xi) = if (lo == INF) false else los
        }
        y += 1
      }
      v(xi * n + xi) = 0.0; s(xi * n + xi) = false
      DBM.close(n, v, s)
      new Zone(names, v, s)
    }

    def withClock(x: QName): Zone =
      if (indexOf(x) >= 0) reset(x)
      else {
        val newNames = orderClocks(names.toList :+ x)
        val m = newNames.length
        val map = newNames.map(c => if (c == x) -1 else indexOf(c))
        val v = new Array[Double](m * m); val s = new Array[Boolean](m * m)
        var i = 0
        while (i < m) {
          var j = 0
          while (j < m) {
            val oi = if (map(i) < 0) 0 else map(i)
            val oj = if (map(j) < 0) 0 else map(j)
            if (i == j) { v(i * m + j) = 0.0; s(i * m + j) = false }
            else { v(i * m + j) = vals(oi * n + oj); s(i * m + j) = strs(oi * n + oj) }
            j += 1
          }
          i += 1
        }
        new Zone(newNames, v, s)
      }

    def withoutClock(x: QName): Zone = {
      val xi = indexOf(x)
      if (xi < 0 || x == ZERO_CLOCK) this
      else {
        val keep = (0 until n).filter(_ != xi).toArray
        val m = keep.length
        val v = new Array[Double](m * m); val s = new Array[Boolean](m * m)
        var i = 0
        while (i < m) {
          var j = 0
          while (j < m) { v(i * m + j) = vals(keep(i) * n + keep(j)); s(i * m + j) = strs(keep(i) * n + keep(j)); j += 1 }
          i += 1
        }
        new Zone(keep.map(names), v, s)
      }
    }

    def extrapolate(maxConst: Map[QName, Double]): Zone = {
      val k = names.map(c => if (c == ZERO_CLOCK) 0.0 else maxConst.getOrElse(c, 0.0))
      var v: Array[Double] = null; var s: Array[Boolean] = null
      var i = 0
      while (i < n) {
        var j = 0
        while (j < n) {
          if (i != j) {
            val b = vals(i * n + j)
            if (i != 0 && b != INF && b > k(i)) {
              if (v == null) { v = vals.clone(); s = strs.clone() }
              v(i * n + j) = INF; s(i * n + j) = false
            } else if (j != 0 && b < -k(j)) {
              if (v == null) { v = vals.clone(); s = strs.clone() }
              v(i * n + j) = norm(-k(j)); s(i * n + j) = true
            }
          }
          j += 1
        }
        i += 1
      }
      if (v == null) this
      else { DBM.close(n, v, s); new Zone(names, v, s) }
    }
  }

  private def satisfiable(n: Int, v: Array[Double], s: Array[Boolean]): Boolean = {
    var i = 0
    while (i < n) {
      val d = v(i * n + i)
      if (!(d > 0 || (d == 0 && !s(i * n + i)))) return false
      i += 1
    }
    true
  }

  private def close(n: Int, v: Array[Double], s: Array[Boolean]): Unit = {
    var k = 0
    while (k < n) {
      var i = 0
      while (i < n) {
        val ikv = v(i * n + k)
        if (ikv != INF) {
          val iks = s(i * n + k)
          var j = 0
          while (j < n) {
            val kjv = v(k * n + j)
            if (kjv != INF) {
              val cv = norm(ikv + kjv); val cs = iks || s(k * n + j)
              if (tighter(cv, cs, v(i * n + j), s(i * n + j))) { v(i * n + j) = cv; s(i * n + j) = cs }
            }
            j += 1
          }
        }
        i += 1
      }
      k += 1
    }
  }

  def initial(clocks: Set[QName]): Zone = {
    val names = orderClocks(clocks)
    val n = names.length
    new Zone(names, Array.fill(n * n)(0.0), new Array[Boolean](n * n))
  }
}
