package patterns
import breeze.linalg.DenseVector

/**
  * Created by valerij on 11/30/16.
  */
class NoPattern extends CurvePattern {
  override def getXSize: Int = 1

  override def getY(x: Int): Double = 1

  override def convolution(yValues: DenseVector[Double]): Double = yValues(0)
}

object NoPattern extends PatternFactory {
  override def apply(windowSize: Int): CurvePattern = new NoPattern
}
