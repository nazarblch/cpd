package patterns

import breeze.linalg.DenseVector

class TrianglePattern(val length: Int) extends CurvePattern {

  val xrange1: DenseVector[Double] = DenseVector.range(0, length/2).map(_.toDouble)
  val xrange2: DenseVector[Double] = DenseVector[Int](Array.range(length/2, length).reverse.map(_ - length/2)).map(_.toDouble)
  val xrange: DenseVector[Int] = DenseVector.range(0, length)
  val norm: Double = xrange.foldLeft(0)((sum, x) => sum + x * x).toDouble

  var height: Double = 0.0
  var alpha: Double = 0.0

  override def getXRange: DenseVector[Int] = xrange

  override def fitParameters(yValues: DenseVector[Double]): Double = {
    assert(yValues.length == length)
    val y1: DenseVector[Double] = yValues.slice(0, length/2)
    val y2: DenseVector[Double] = yValues.slice(length/2, length)

    val y1x1: Double = y1 dot xrange1
    val y2x2: Double = y2 dot xrange2

    alpha = (y1x1 + y2x2) / norm
    height = alpha * length / 2.0

    height
  }

  override def getY(x: Int): Double = {
    if (x < length / 2) alpha * x else (length - x) * alpha
  }

  override def getY: DenseVector[Double] = xrange.map(x => {
    if (x < length / 2) alpha * x else (length - x) * alpha
  })

  override def getXSize: Int = length
}
