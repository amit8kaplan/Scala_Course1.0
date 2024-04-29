import scala.math.abs
import Util._

class Line(ps: Array[Point]) {
  require(ps.length >= 2, "At least two points are required for a line.")

  val a = covariance(ps.map(_.x), ps.map(_.y)) / variance(ps.map(_.x))
  val b = mu(ps.map(_.y)) - a * mu(ps.map(_.x))
  def getA: Double = a
  def getB: Double = b

  def f(x: Double): Double = a * x + b
  def dist(p: Point): Double = abs(f(p.x) - p.y)

}
