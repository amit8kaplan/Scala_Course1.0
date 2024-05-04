import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Util {

  def max[A](list: List[A], comparator: (A, A) => Int): A = {
    if (list.isEmpty) throw new IllegalArgumentException("List cannot be empty")
    var maxElement = list.head
    for (element <- list.tail) {
      if (comparator(element, maxElement) > 0) {
        maxElement = element
      }
    }
    maxElement
  }

  def map[A, B, C](list: List[A], func1: A => B, func2: B => C): List[C] = {
    list.map(func1 andThen func2)
  }

  def isSorted[A](list: List[A], comparator: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def isSortedRec(list: List[A]): Boolean = list match {
      case Nil => true
      case _ :: Nil => true
      case x :: y :: xs => comparator(x, y) && isSortedRec(y :: xs)
    }

    isSortedRec(list)
  }

  def probs(xs: Array[Double]): Array[Double] = {
    val frequencies = xs.groupBy(identity).mapValues(_.length)
    val totalSum = frequencies.values.sum.toDouble
    xs.map(x => frequencies(x) / totalSum)
  }

  def entropy(xs: Array[Double]): Double = {
    val frequencies = xs.groupBy(identity).mapValues(_.length.toDouble)
    val totalSum = frequencies.values.sum
    val probabilities = frequencies.map { case (_, freq) =>
      freq / totalSum
    }
    val entropy = probabilities.map { p =>
      if (p != 0) -p * math.log(p) / math.log(2) else 0
    }.sum
    entropy
  }

  def mu(xs: Array[Double]): Double = {
    xs.sum / xs.length.toDouble
  }

  def variance(xs: Array[Double]): Double = {
    val mean = mu(xs)
    val squaredDifferences = xs.map(x => math.pow(x - mean, 2))
    squaredDifferences.sum / xs.length.toDouble
  }

  def zscore(xs: Array[Double], value: Double): Double = {
    val stdDev = math.sqrt(variance(xs))
    (value - mu(xs)) / stdDev
  }

  def covariance(xs: Array[Double], ys: Array[Double]): Double = {
    require(xs.length == ys.length, "Arrays xs and ys must have the same length")
    val meanX = mu(xs)
    val meanY = mu(ys)
    val cov = (xs zip ys).map { case (x, y) =>
      (x - meanX) * (y - meanY)
    }
    mu(cov)
  }

  def pearson(xs: Array[Double], ys: Array[Double]): Double = {
    val cov = covariance(xs, ys)
    val stdDevX = math.sqrt(variance(xs))
    val stdDevY = math.sqrt(variance(ys))
    cov / (stdDevX * stdDevY)
  }

  def pearsonFromTimeSeries(normal: TimeSeries): Seq[(String, String, Double)] = {
    normal.features.zipWithIndex.flatMap { case (feature1, index) =>
      normal.features.zipWithIndex
        .drop(index + 1)
        .map { case (feature2, _) =>
          val correlation = Util.pearson(
            normal.getValues(feature1).getOrElse(Vector.empty[Double]).toArray,
            normal.getValues(feature2).getOrElse(Vector.empty[Double]).toArray
          ).abs
          (feature1, feature2, correlation)
        }
    }
  }

  def distanceBetweenPoints(p1x: Double, p1y: Double, p2x: Double, p2y: Double): Double = {
    Math.sqrt(Math.pow(p1x - p2x, 2) + Math.pow(p1y - p2y, 2))
  }
}
