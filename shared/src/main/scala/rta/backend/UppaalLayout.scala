package rta.backend

trait UppaalLayout {
  def getPos(id: String): (Double, Double)
  def getNails(sourceId: String, targetId: String, edgeId: String): List[(Double, Double)]
}

object EmptyLayout extends UppaalLayout {
  def getPos(id: String): (Double, Double) = (0.0, 0.0)
  def getNails(sourceId: String, targetId: String, edgeId: String): List[(Double, Double)] = Nil
}