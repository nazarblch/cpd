package generate

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.{Dirichlet, MultivariateGaussian}

class DirichletDataGenerator(val alpha: DenseVector[Double])
  extends DataGenerator[DenseVector[Double], DenseVector[Double]] {

  var r = Dirichlet(alpha)

  override def next: DenseVector[Double] = r.draw()

  override def update(param: DenseVector[Double]): Unit = {
    r = Dirichlet(param)
  }
}