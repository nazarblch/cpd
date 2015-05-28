package patterns

import breeze.linalg.DenseVector


trait CurvePattern {
  def getXRange: DenseVector[Int]
  def getXSize: Int
  def getY(x: Int): Double
  // def getYRange: DenseVector[Double]
  def fitParameters(yValues: DenseVector[Double]): Double
  def getPlot(offset: Int, length: Int): DenseVector[Double] = {

    assert(offset + getXSize <= length)

    val res = DenseVector.fill[Double](length)(getY(0))

    for (i <- 0 until getXSize) {
      res(i + offset) = getY(i)
    }

    res
  }

  def convolution(yValues: DenseVector[Double]): Double
}

class MatcherResult(val offset: Int, val pattern: CurvePattern) {
}


object PatternMatcher {
  def exec(pattern: CurvePattern, data: DenseVector[Double]): MatcherResult = {
    // fixme: prevent slicing

    val offset: Int =
      (0 until (data.length - pattern.getXSize)).map(i => {
        val ys = data.slice(i, pattern.getXSize + i)
        (i, pattern.fitParameters(ys))
      }).maxBy(_._2)._1

    pattern.fitParameters(data.slice(offset, offset + pattern.getXSize))

    new MatcherResult(offset, pattern)

  }

  def exec(pattern: CurvePattern, data: Array[Double]): MatcherResult = {
    exec(pattern, DenseVector(data))
  }

  def convolution(pattern: CurvePattern, data: DenseVector[Double]): Array[Double] = {

    (0 until (data.length - pattern.getXSize)).map(i => {
      val ys = data.slice(i, pattern.getXSize + i)
      pattern.fitParameters(ys)
      val conv = pattern.convolution(ys)
      conv
    }).toArray

  }
}
