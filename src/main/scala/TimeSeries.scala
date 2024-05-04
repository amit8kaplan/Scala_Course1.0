import scala.io.Source

class TimeSeries(csvFileName: String) {
  // Read data from CSV file
    val newcsvFileName = "C:\\Users\\amit8\\Documents\\collage\\functional_course\\solutions\\1\\1\\target\\scala-3.3.3\\classes\\" + csvFileName
    private val source = Source.fromFile(newcsvFileName)
//  private val source = Source.fromInputStream(getClass.getResourceAsStream(csvFileName))
  val lines: Vector[String] = {
    //println("Reading lines from CSV file..." + csvFileName)
    val lines = source.getLines().toVector
    //println("Lines read successfully." + lines.length + " lines read.")
    //println("line   " + lines)
    lines
  }
  source.close()
  // Extract features from the first line
  val features: Vector[String] = {
    //println("\n\n")
    //println("Extracting features...")
    val features = lines.head.split(",").toVector
    //println("Features extracted successfully." + features.length + " features extracted.")
    //println("features   " + features)
    features
  }

  // Initialize data map excluding the header line
  private val data: Map[String, Vector[Double]] = {
    //println("\n\n")
    //println("Initializing data map...")
    val dataLines = lines.tail
    val dataMap = features.zipWithIndex.map { case (feature, index) =>
      feature -> dataLines.map(_.split(",")(index).toDouble)
    }.toMap
    //println("Data map initialized successfully." + dataMap.size + " features initialized.")
    //println("dataMap   " + dataMap)
    dataMap
  }

  // Map to store feature indices
  private val featureIndices: Map[String, Int] = {
    //println("\n\n")
    //println("Initializing feature indices map...")
    val featureIndicesMap = features.zipWithIndex.toMap
    //println("Feature indices map initialized successfully." + featureIndicesMap.size + " features initialized.")
    //println("featureIndicesMap   " + featureIndicesMap)
    featureIndicesMap
  }

  // Given name of a feature, return its value series in O(1)
  def getValues(feature: String): Option[Vector[Double]] = {
    //println("\n\n")
    //println(s"Getting values for feature: $feature")
    val res = data.get(feature)
    //println("res   " + res)
    res
  }

  // Given name of a feature and a time step, return its value at the given time step in O(1)
  def getValue(feature: String, timeStep: Int): Option[Double] = {
    //println("\n\n")
    //println(s"Getting value for feature: $feature at time step: $timeStep")
    val res = for {
      index <- featureIndices.get(feature)
      values <- data.get(feature)
      if timeStep >= 0 && timeStep < values.length
    } yield values(timeStep)
    //println("res   " + res)
    res
  }

  // Given name of a feature, return its value series in the range of indices in O(1)
  def getValues(feature: String, r: Range): Option[Vector[Double]] = {
    ////println("\n\n")
    //println(s"Getting values for feature: $feature in range: $r")

    // Check if the range is out of bounds
    if (r.start < 0 || r.end >= lines.length-1) {
      //println("Range is out of bounds. Returning None.")
      return None
    }

    val res = for {
      index <- featureIndices.get(feature)
      values <- data.get(feature)
    } yield {
      val mappedValues = r.map(i => values(i)).toVector
      Some(mappedValues)
    }
    //println("res   " + res)
    res.flatten // Flatten the Option[Option[Vector[Double]]] to Option[Vector[Double]]
  }

  //  // Given name of a feature, return its value series in the range of indices in O(1)
//  def getValues(feature: String, r: Range): Option[Vector[Double]] = {
//    //println("\n\n")
//    //println(s"Getting values for feature: $feature in range: $r")
//    val res= for {
//      index <- featureIndices.get(feature)
//      values <- data.get(feature)
//    } yield r.map(i => if (i >= 0 && i < values.length) values(i) else Double.NaN).toVector
//    //println("res   " + res)
//    res
//  }

}



//import scala.io.Source
//import scala.collection.mutable
//import scala.collection.immutable
//
//class TimeSeries(csvFileName:String) {
//  val newcsvFileName = "C:\\Users\\amit8\\Documents\\collage\\functional_course\\solutions\\1\\1\\target\\scala-3.3.3\\classes\\" + csvFileName
//
//  // Read lines from file
//  private val source = Source.fromFile(newcsvFileName)
//  private val lines = source.getLines()
//
//  // Split content to header and data
//  val features: Array[String] = lines.take(1).next.split(",")
//  private val features_map: Map[String, Int] = features.zipWithIndex.map(x=>(x._1,x._2)).toMap[String,Int]
//
//  // Build the VectorMap
//  private val tmp_data = Vector.newBuilder[Vector[Double]]
//
//  // Initialize VectorMap builder for each feature
//
//  private val tmp_arr = features.map((_,Vector.newBuilder[Double]))
//
//  // Fill the columns to the sub vector builders
//  lines.zipWithIndex.foreach(line =>
//    line._1.split(",").map(x=>x.toDouble).zipWithIndex.map(value=>{
//      tmp_arr(value._2)._2+=(value._1.toDouble)
//    })
//  )
//
//  tmp_arr.zipWithIndex.foreach(x => tmp_data+=(
//    (features(x._2), x._1._2.result())._2
//    ))
//
//  private val data = tmp_data.result()
//
//
//  source.close()
//
//  // given name of a feature return in O(1) its value series
//  def getValues(feature:String):Option[Vector[Double]]= {
//    if (!features_map.contains(feature))
//      return None
//
//    Option(data(features_map(feature)))
//  }
//
//  // given name of a feature return in O(1) its value at the given time step
//  def getValue(feature:String,timeStep:Int):Option[Double]= {
//    if (!features_map.contains(feature))
//      return None
//
//    val f = data(features_map(feature))
//    if (timeStep >= f.length)
//      return None
//    Option(f(timeStep))
//  }
//
//  // given name of a feature return its value series in the range of indices
//  def getValues(feature:String,r:Range):Option[Vector[Double]]={
//    if (!features_map.contains(feature))
//      return None
//
//    val f = data(features_map(feature))
//    if (r.head < 0 || r.head >= f.length || r.last >= f.length)
//      return None
//
//    val len = f.size
//    Option(f.drop(r.head).dropRight(len - r.last - 1))
//  }
//}
