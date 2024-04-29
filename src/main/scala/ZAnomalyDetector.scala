object ZAnomalyDetector extends AnomalyDetector {

  override def learn(normal: TimeSeries): Map[String, String] = {
    val thresholds = normal.features.map { feature =>
      val column = normal.getValues(feature).getOrElse(Vector.empty[Double])
      val maxZScore = column.map(value => math.abs(Util.zscore(column.toArray, value))).max
      feature -> maxZScore.toString
    }.toMap
    thresholds.view.mapValues(_.toString).toMap
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    val anomalies = for {
      (feature, threshold) <- model.toVector
      column = test.getValues(feature).getOrElse(Vector.empty[Double])
      (value, index) <- column.zipWithIndex
      zScore = math.abs(Util.zscore(column.toArray, value))
      if zScore > threshold.toDouble
    } yield (feature, index)
    anomalies
  }
}
