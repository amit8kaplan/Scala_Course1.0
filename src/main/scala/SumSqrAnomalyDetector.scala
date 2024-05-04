
import scala.collection.mutable
import Util.distanceBetweenPoints
object SumSqrAnomalyDetector extends  AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    val resultMap = Util.pearsonFromTimeSeries(normal)
      .filter(_._3 >0.9)
      .collect {
      case (col1: String, col2: String, _) =>
        val xValues = normal.getValues(col1).get.toArray
        val yValues = normal.getValues(col2).get.toArray
        val points = xValues.zip(yValues).map { case (x, y) => Point(x, y) }
        val maxDist = points.map { p1 =>
          points.filter(_ != p1).map { p2 =>
            distanceBetweenPoints(p1.x, p1.y, p2.x, p2.y)
          }.sum
        }.max
        ("" + col1 + "," + col2) -> ("" + maxDist)
    }.toMap
    resultMap
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    val returned_vector = model.map {
      case (itemKey, itemValue) =>
      val Array(feature1, feature2) = itemKey.split(",")
        val points = for {
          (x, y) <- test.getValues(feature1).get.toArray zip test.getValues(feature2).get.toArray
        } yield new Point(x, y)
        points.zipWithIndex.flatMap { case (p1, index) =>
          val dist = points.zipWithIndex.collect {
            case (p2, i) if i != index => distanceBetweenPoints(p1.x, p1.y ,p2.x, p2.y)
          }.sum
          Option.when(dist > itemValue.toDouble)((feature1 + "," + feature2, index))
      }
    }.flatten.toVector
    returned_vector
  }
}