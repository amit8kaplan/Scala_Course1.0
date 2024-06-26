
object MainTrain {

  // a simple test for time series
  def testTimeSeries():Unit={
    val ts=new TimeSeries("train.csv")
    val a=Vector(1,2,3,4)
    val b=Vector(3,4,5)

    // Test for non-existent key
    if(ts.getValues("E")!=None)
      println("when key does not exist you should return None (-1)")

    // Test for valid key
    if(!ts.getValues("A").get.sameElements(a) && !ts.getValues("A").get.isInstanceOf[Vector[Double]])
      println("problem with getValues (-2)")

    // Test for valid key and range
    if(!ts.getValues("B",1 to 3).get.sameElements(b))
      println("problem with getValues with range (-3)")


    // Test for out of bounds index range but valid key
    if(ts.getValues("B",0 to 4)!=None || ts.getValues("B",-1 to 3)!=None)
      println("when index is out of bounds you should return None (-1)")

    // Test for valid key and time step
    if(ts.getValue("C",2).get!=5)
      println("problem with get Value (-2)")

    // Test for out of bounds index but valid key
    if(ts.getValue("D",5)!=None)
      println("when index is out of bounds you should return None (-1)")

      // Additional edge cases
      // Test for empty feature name
      if (ts.getValues("").isDefined)
        println("Empty feature name should return None")

      // Test for empty range
      if (ts.getValues("A", 5 to 3).isDefined)
        println("Empty range should return None")

      // Test for out of bounds time step with empty data
      val emptyTs = new TimeSeries("empty.csv")
      if (emptyTs.getValue("A", 0).isDefined)
        println("Empty time series should return None")

      // Test for out of bounds range with empty data
      if (emptyTs.getValues("A", 0 to 1).isDefined)
        println("Empty time series should return None for range")
  }


  // ZAnomalyDetector test
  def testZAD():Unit={
    val ts=new TimeSeries("train2.csv")
    val model=ZAnomalyDetector.learn(ts)

    val r0=ZAnomalyDetector.detect(model,ts)
    if(r0.length>0)
      println("there should not be any anomalies detected here (-10)")
//    else
//      println("no anomalies detected in training data (+10)" + r0.length)
    val r1=ZAnomalyDetector.detect(model,new TimeSeries("test2.csv"))
    if(r1.length!=1)
      println("there should be exactly one anomaly reported here (-10)")
//    else
//      println("one anomaly detected in test data (+10)" + r1.length)
    if(r1(0)._1!="A" || r1(0)._2!=19)
      println("wrong anomaly detected (-10)")
//    else
//      println("correct anomaly detected (+10)"+r1(0)._1 + r1(0)._2)

  }



  // LinearRegAnomalyDetector test
  def testLRAD():Unit={
    println("testLRAD")
    val ts=new TimeSeries("train3.csv")
    val model=LinearRegAnomalyDetector.learn(ts)
    val r0=LinearRegAnomalyDetector.detect(model,ts)
//    println("Model" + model)
//    println("R0" +r0)

    if(r0.length>0)
      println("there should not be any anomalies detected here (-10)")
//    else
//      println("no anomalies detected in training data (+10)" + r0.length)
    val r1=LinearRegAnomalyDetector.detect(model,new TimeSeries("test3.csv"))
//    println("R1" + r1)
    if(r1.length!=2)
      println("wrong number of reported anomalies (-10)")
//      println("")
//      println("R1" + r1)
//      println("")
//      println("R1,length" + r1.length)
//      println()
//    else
//      println("correct number of anomalies detected in test data (+10)" + r1.length)
    if(!r1.contains(("A,B",5)) || !r1.contains(("C,D",13)))
      println("wrong anomalies reported (-10)")
//    else
//      println("correct anomalies detected (+10)" + r1.contains(("A,B",5)) + r1.contains(("C,D",13)))
  }

  // SumSqrAnomalyDetector test
  def testSSD():Unit={
    val ts=new TimeSeries("train3.csv")
    val model=SumSqrAnomalyDetector.learn(ts)
    val r0=SumSqrAnomalyDetector.detect(model,ts)
    if(r0.length>0)
      println("there should not be any anomalies detected here (-10)")
    val r1=SumSqrAnomalyDetector.detect(model,new TimeSeries("test3.csv"))
    if(r1.length!=2)
      println("wrong number of reported anomalies (-10)")
    if(!r1.contains(("A,B",5)) || !r1.contains(("A,B",18)))
      println("wrong anomalies reported (-10)")

  }
  // HybridAnomalyDetector test
  def testHAD():Unit={
    val ts=new TimeSeries("train4.csv")
    val model=HybridAnomalyDetector.learn(ts)
    println(model)
    val r0=HybridAnomalyDetector.detect(model,ts)

    if(r0.length>0)
      println("there should not be any anomalies detected here (-10)")
    val r1=HybridAnomalyDetector.detect(model,new TimeSeries("test4.csv"))


    if(r1.length!=5) {
      println("wrong number of reported anomalies (-10)")
      println(r1)
    }
    else
      {println("correct number of anomalies detected in test data (+10)" + r1.length)
      println(r1)}
    if(!r1.contains(("E",24)) || !r1.contains(("B",10)) || !r1.contains(("D",15)) || !r1.contains(("A,B",10)) || !r1.contains(("C,D",15))) {
      println("wrong anomalies reported (-10)")
      println(r1)
    }
    else
      {
        println(r1)
      }
  }

  def main(args: Array[String]): Unit = {

    testTimeSeries()
    testZAD()
    testLRAD()
    println("testSSD")
    testSSD()
    println("testHyb")

    testHAD()

    println("done")
  }
}
