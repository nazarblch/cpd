package likelihood_optimize

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.optimize._
import datasets.CellT._
import datasets.{DataHeader, WeightedDataset, Dataset}
import models.ParametricModel




class AdaptiveGradientDescentOptimizer[Row, Self <: Dataset[Row, Self]](
                                                                         val init: DenseVector[Double],
                                                                         val model: ParametricModel[Row, Self, DenseVector[Double]])
     extends ParametricModel[Row, Self, DenseVector[Double]] {

  // fixme: add P parameter for model

  override def MLE(dataset: WeightedDataset[Row, Self]): DenseVector[Double] = {
    val opt = FirstOrderMinimizer.OptParams(batchSize = 512, regularization = 0.0, alpha = 0.7, maxIterations = -1,
      useL1 = false,
      tolerance = 1E-4,
      useStochastic = false)

    val f= new DiffFunction[DenseVector[Double]] {
                     def calculate(x: DenseVector[Double]) = {
                         (-likelihood(dataset, x), -gradLikelihood(dataset, x))
                       }
                   }

    opt.minimize[DenseVector[Double]](f, init)
  }

  def likelihood(dataset: WeightedDataset[Row, Self], parameter: DenseVector[Double]): Double = model.likelihood(dataset, parameter)

  def dim: Int = model.dim

  def fisherMatrix(dataset: WeightedDataset[Row, Self]): DenseMatrix[Double] = null

  def gradLikelihood(dataset: WeightedDataset[Row, Self], parameter: DenseVector[Double]): DenseVector[Double] = model.gradLikelihood(dataset, parameter)

  def header: DataHeader = model.header

}


