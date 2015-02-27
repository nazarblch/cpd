package generate

import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.stats.distributions.{Gaussian, MultivariateGaussian}


class NormalDataGenerator(val mean: DenseVector[Double])
    extends DataGenerator[DenseVector[Double], DenseVector[Double]] {

  val covariance: DenseMatrix[Double] = DenseMatrix.eye(mean.length)
  var r = MultivariateGaussian(mean, covariance)

  def updateMean(m: DenseVector[Double]): Unit = {
    assert(mean.length == m.length)
    r = MultivariateGaussian(m, covariance)
  }

  override def next: DenseVector[Double] = r.draw()

  override def update(param: DenseVector[Double]): Unit = updateMean(param)
}


class ScalarNormalDataGenerator(val mean: Double = 0, val covariance: Double = 1)
  extends DataGenerator[Double, (Double, Double)] {

  var r = Gaussian(mean, covariance)

  override def next: Double = r.draw()

  override def update(param: (Double, Double)): Unit = {
    println("m=" + param._1 + " s^2=" + param._2)
    r = Gaussian(param._1, param._2)
  }
}
