package patterns

import breeze.linalg.DenseVector
import breeze.stats._

class HalfTrianglePattern(val length: Int) extends CurvePattern {

  val xrange: DenseVector[Int] = DenseVector.range(0, length)

  val x_mean = (length.toDouble - 1) / 2
  val xrangeCentered: DenseVector[Double] = xrange.map(x => x - x_mean)

  override def getXRange: DenseVector[Int] = xrange

  val norm: Double = xrangeCentered.foldLeft(0.0)((sum, x) => sum + x * x)

  var alpha: Double = 0.0
  var y_min: Double = 0

  override def fitParameters(yValues: DenseVector[Double]): Double = {
    assert(yValues.length == length)

    val y1x1: Double = yValues dot xrangeCentered

    alpha = y1x1 / norm
    if (alpha < 0) alpha = 0

    y_min = mean(yValues) - alpha * x_mean

    convolution(yValues)
  }

  override def getY(x: Int): Double = {
    assert(x < length && x >= 0)
    alpha * x + y_min
  }

  def getYRange: DenseVector[Double] = xrange.map(x => {
    getY(x)
  })

  override def getXSize: Int = length

  override def convolution(yValues: DenseVector[Double]): Double = (yValues - y_min) dot (getYRange - y_min)

}


class StaticHalfTrianglePattern(val length: Int) extends CurvePattern {

  val xrange: DenseVector[Int] = DenseVector.range(0, length)

  val x_mean = length.toDouble / 2

  override def getXRange: DenseVector[Int] = xrange

  override def fitParameters(yValues: DenseVector[Double]): Double = {
    assert(yValues.length == length)

    getYRange dot yValues
  }

  override def getY(x: Int): Double = {
    assert(x < length && x >= 0)
    (x + 1).toDouble / length
  }

  def getYRange: DenseVector[Double] = xrange.map(x => {
    getY(x)
  })

  override def getXSize: Int = length

  override def convolution(yValues: DenseVector[Double]): Double = yValues dot getYRange
}

