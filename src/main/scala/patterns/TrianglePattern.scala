package patterns

import breeze.linalg.DenseVector
import breeze.stats._

class TrianglePattern(val length: Int) extends CurvePattern {

  val x_mean: Double = (length.toDouble - 1) / 4
  val xrange1: DenseVector[Double] = DenseVector.range(0, length/2).map(_.toDouble - x_mean)
  val xrange: DenseVector[Int] = DenseVector.range(0, length)

  val norm: Double = xrange1.foldLeft(0.0)((sum, x) => sum + x * x).toDouble

  var height: Double = 0.0
  var alpha: Double = 0.0
  var y_min: Double = 0

  override def getXRange: DenseVector[Int] = xrange

  override def fitParameters(yValues: DenseVector[Double]): Double = {
    assert(yValues.length == length)
    val y1: DenseVector[Double] = yValues.slice(0, length/2)
    val y2: DenseVector[Double] = DenseVector[Double](yValues.slice(length/2, length).toArray.reverse)

    val y1x1: Double = y1 dot xrange1
    val y2x2: Double = y2 dot xrange1

    alpha = 0.5 * (y1x1 + y2x2) / norm
    y_min = mean(yValues) - alpha * x_mean

    height = alpha * length / 2.0 + y_min

    // println(alpha)

    if (alpha < 0) alpha = 0

    convolution(yValues)

    //alpha * length / 2.0
  }

  override def getY(x: Int): Double = {
    (if (x < length / 2) alpha * x else (length - x) * alpha ) + y_min
  }

  def getYRange: DenseVector[Double] = xrange.map(x => {
    getY(x)
  })

  override def getXSize: Int = length

  override def convolution(yValues: DenseVector[Double]): Double = (yValues - y_min ) dot (getYRange - y_min )

}

object TrianglePattern extends PatternFactory {
  override def apply(windowSize: Int): TrianglePattern = new TrianglePattern(2 * windowSize)
}


class StaticTrianglePattern(val length: Int) extends CurvePattern {

  val x_mean: Double = length.toDouble / 4
  val xrange1: DenseVector[Double] = DenseVector.range(0, length/2).map(_.toDouble - x_mean)
  val xrange: DenseVector[Int] = DenseVector.range(0, length)

  val norm: Double = xrange1.foldLeft(0.0)((sum, x) => sum + x * x)

  var height: Double = length / 2
  var alpha: Double = 2 * height / length
  var y_min: Double = - length / 4

  override def getXRange: DenseVector[Int] = xrange

  override def fitParameters(yValues: DenseVector[Double]): Double = {
    yValues dot getYRange
  }

  override def getY(x: Int): Double = {
    (if (x < length / 2) alpha * x else (length - x) * alpha ) + y_min
  }

  def getYRange: DenseVector[Double] = xrange.map(x => {
    getY(x)
  })

  override def getXSize: Int = length

  override def convolution(yValues: DenseVector[Double]): Double = (yValues ) dot (getYRange )
}

object StaticTrianglePattern extends PatternFactory {
  override def apply(windowSize: Int): StaticTrianglePattern = new StaticTrianglePattern(2 * windowSize)
}