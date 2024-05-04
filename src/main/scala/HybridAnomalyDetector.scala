
import Util.distanceBetweenPoints

import scala.collection.mutable

object HybridAnomalyDetector extends AnomalyDetector {

  override def learn(normal: TimeSeries): Map[String, String] = {
    val resultsMap = Map.newBuilder[String, String]
    val pearsonResults = Util.pearsonFromTimeSeries(normal).toList // Convert sequence to list
    pearsonResults.foreach { case (feature1, feature2, correlation) =>
      if (correlation > 0.9) {
        val x = normal.getValues(feature1).getOrElse(Vector.empty[Double]).toArray
        val y = normal.getValues(feature2).getOrElse(Vector.empty[Double]).toArray
        val points = x.zip(y).map { case (xi, yi) => new Point(xi, yi) }.toArray
        val line = new Line(points)
        val maxDist = points.map(p => Math.abs(line.dist(p))).max
        resultsMap += (("" + "LRAD," + feature1 + "," + feature2, "" + line.a + "," + line.b + "," + maxDist))
      }
      else if (correlation > 0.5 && correlation < 0.9) {
        val x = normal.getValues(feature1).getOrElse(Vector.empty[Double]).toArray
        val y = normal.getValues(feature2).getOrElse(Vector.empty[Double]).toArray
        val points = x.zip(y).map { case (xi, yi) => new Point(xi, yi) }.toArray

        val centerPoint = points.minBy(p1 =>
          points.filter(_ != p1).map(p2 =>
            Util.distanceBetweenPoints(p1.x, p1.y, p2.x, p2.y)
          ).sum
        )
        val maxDist = points.filter(_ != centerPoint).map(p2 =>
          Util.distanceBetweenPoints(centerPoint.x, centerPoint.y, p2.x, p2.y)
        ).max
        resultsMap += (("" + "SSD," + feature1 + "," + feature2, "" + maxDist))
      }
      else {
        val arr1 = normal.getValues(feature1).getOrElse(Vector.empty[Double]).toArray
        val max1 = arr1.map(currNum => Util.zscore(arr1, currNum).abs).max
        resultsMap += (("" + "ZAD," + feature1, "" + max1))

        val arr2 = normal.getValues(feature2).getOrElse(Vector.empty[Double]).toArray
        val max2 = arr2.map(currNum => Util.zscore(arr2, currNum).abs).max
        resultsMap += (("" + "ZAD," + feature2, "" + max2))
      }
    }
    resultsMap.result()
  }


  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var resultVec: Vector[(String, Int)] = Vector.empty

    model.foreach { case (lineKey, lineValue) =>
      val parts = lineKey.split(",")
      if (parts(0) == "LRAD") {
        val feature1 = parts(1)
        val feature2 = parts(2)
        val (a, b, maxDist) = lineValue.split(",").map(_.toDouble) match {
          case Array(a, b, maxDist) => (a, b, maxDist)
        }
        val feature1Values = test.getValues(feature1).getOrElse(Vector.empty[Double])
        val feature2Values = test.getValues(feature2).getOrElse(Vector.empty[Double])
        val outliers = feature1Values.zip(feature2Values).zipWithIndex.filter { case ((x, y), _) =>
          val res = Math.abs(Math.abs(y) - Math.abs(x * a + b))
          res > maxDist
        }.map(_._2)
        resultVec ++= outliers.map(index => (s"$feature1,$feature2", index))
      }
      else if (parts(0) == "SSD") {
        val feature1 = parts(1)
        val feature2 = parts(2)
        val maxDist = lineValue.toDouble

        val X = test.getValues(feature1).getOrElse(Vector.empty[Double])
        val Y = test.getValues(feature2).getOrElse(Vector.empty[Double])

        val pointsArr = X.zip(Y).map { case (x, y) => new Point(x, y) }.toArray
        val centerPoint = pointsArr.minBy(p1 =>
          pointsArr.filter(_ != p1).map(p2 =>
            Util.distanceBetweenPoints(p1.x, p1.y, p2.x, p2.y)
          ).sum
        )

        val maxDistFound = pointsArr.zipWithIndex
          .filter { case (p, _) => p != centerPoint }
          .map { case (p, index) =>
            (index, Util.distanceBetweenPoints(centerPoint.x, centerPoint.y, p.x, p.y))
            //            (index, Math.sqrt(Math.pow(centerPoint.x - p.x, 2) + Math.pow(centerPoint.y - p.y, 2)))
          }.maxBy(_._2)

        if (maxDistFound._2 > maxDist) {
          resultVec :+= ((s"$feature1,$feature2", maxDistFound._1))
        }

      } else if (parts(0) == "ZAD") {
        val feature = parts(1)
        val maxZScore = lineValue.toDouble

        val arr = test.getValues(feature).getOrElse(Vector.empty[Double]).toArray
        val maxFound = arr.zipWithIndex.map { case (value, index) => (Util.zscore(arr, value).abs, index) }.maxBy(_._1)

        if (maxFound._1 > maxZScore) {
          resultVec :+= ((feature, maxFound._2))
        }
      }
    }

    resultVec
  }


}
