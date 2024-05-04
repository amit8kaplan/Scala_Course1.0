import scala.collection.mutable
object LinearRegAnomalyDetector extends  AnomalyDetector {

  override def learn(normal: TimeSeries): Map[String, String] = {
    val model = mutable.Map[String, String]()
    Util.pearsonFromTimeSeries(normal)
      .filter(_._3 > 0.9)
      .foreach(pair => {
        val x = normal.getValues(pair._1).get.toArray
        val y = normal.getValues(pair._2).get.toArray
        val points = x.zip(y).map { case (xi, yi) => new Point(xi, yi) }
        val line = new Line(points)
        val maxDist = points.map(p => Math.abs(line.dist(p))).max
        val key = pair._1 + "," + pair._2
        val value = line.a.toString + "," + line.b.toString + "," + maxDist.toString
        model.addOne(key -> value)
      })
    model.toMap
  }


  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var ret: Vector[(String, Int)] = Vector.empty
    model.foreach { case (features, serializedData) =>
      val Array(feature1, feature2) = features.split(",")
      val Array(a, b, maxDist) = serializedData.split(",").map(_.toDouble)
      val line = new Line(Array(new Point(0, b), new Point(1, a + b))) // Create a Line object using two points
      val testData = test.getValues(feature1).get zip test.getValues(feature2).get
      testData.zipWithIndex.foreach { case ((x, y), index) =>
        val dist = line.dist(new Point(x, y)) // Compute distance using the Line class
        if (dist > maxDist) {
          ret = ret :+ (feature1 + "," + feature2, index)
        }
      }
    }
    ret
  }
}