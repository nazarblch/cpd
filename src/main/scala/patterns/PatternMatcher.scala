package patterns

import breeze.linalg.DenseVector


trait CurvePattern {
  def getXRange: DenseVector[Int]
  def getXSize: Int
  def getY(x: Int): Double
  def getY: DenseVector[Double]
  def fitParameters(yValues: DenseVector[Double]): Double
}

class MatcherResult[T <: CurvePattern](val offset: Int, val pattern: T) {
}


object PatternMatcher {
  def exec[T <: CurvePattern](pattern: T, data: DenseVector[Double]): MatcherResult[T] = {
    // fixme: prevent slicing

    val offset: Int =
      (0 until (data.length - pattern.getXSize)).map(i => {
        val ys = data.slice(i, pattern.getXSize + i)
        (i, pattern.fitParameters(ys))
      }).maxBy(_._2)._1

    new MatcherResult[T](offset, pattern)

  }
}
