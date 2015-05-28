package models

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.optimize.FisherMatrix
import datasets.CellT.{TCellDouble, CellType}
import datasets.{Dataset, WeightedDataset, DataHeader}


trait Model[Row, Self <: Dataset[Row, Self]] {

  def header: DataHeader

  def likelihood(dataset: WeightedDataset[Row, Self]): Double

  def likelihood(dataset: Self): Double = likelihood(WeightedDataset(dataset, Vector.fill[Double](dataset.size)(1.0)))
}


trait IIDModel[Row, Self <: Dataset[Row, Self]] extends Model[Row, Self] {

  def likelihood(dataRow: Row): Double
  
  override def likelihood(dataset: WeightedDataset[Row, Self]): Double = {
    assert(header equals dataset.header)
    dataset.convolution(likelihood)
  }
}

trait ParametricModel[Row, Self <: Dataset[Row, Self], P] extends Model[Row, Self] {

  override def likelihood(dataset: WeightedDataset[Row, Self]): Double = {
    assert(header equals dataset.header)
    likelihood(dataset, MLE(dataset))
  }

  def likelihood(dataset: WeightedDataset[Row, Self], parameter: P): Double

  def likelihood(dataset: Self, parameter: P): Double = likelihood(WeightedDataset(dataset, Vector.fill(dataset.size)(1.0)), parameter)

  def dim: Int

  def MLE(dataset: WeightedDataset[Row, Self]): P

  def MLE(dataset: Self, weights: Vector[Double]): P = MLE(WeightedDataset(dataset, weights))

  def MLE(dataset: Self): P = MLE(WeightedDataset(dataset, Vector.fill(dataset.size)(1.0)))

  def fisherMatrix(dataset: WeightedDataset[Row, Self]): DenseMatrix[Double]

  def gradLikelihood(dataset: WeightedDataset[Row, Self], parameter: P): P

  def gradLikelihood(dataset: Self, parameter: P): P = gradLikelihood(WeightedDataset(dataset, Vector.fill(dataset.size)(1.0)), parameter)

}

trait ParametricIIDModel[Row, Self <: Dataset[Row, Self]] extends ParametricModel[Row, Self, DenseVector[Double]] {

  def likelihood(dataRow: Row, parameter: DenseVector[Double]): Double

  override def likelihood(dataset: WeightedDataset[Row, Self], parameter: DenseVector[Double]): Double = {
    dataset.convolution(likelihood(_, parameter))
  }

  def gradLikelihood(dataRow: Row, parameter: DenseVector[Double]): DenseVector[Double]

  override def gradLikelihood(dataset: WeightedDataset[Row, Self], parameter: DenseVector[Double]): DenseVector[Double] = {
    dataset.convolutionV(gradLikelihood(_, parameter))
  }

}


abstract class GeneralRegressionIIDModel[Row, Self <: Dataset[Row, Self]](
                                                           val header: DataHeader,
                                                           val regressionFunction: RegressionFunction[Row, DenseVector[Double]],
                                                           val model: ParametricIIDModel[Row, Self],
                                                           val optimizer: (WeightedDataset[Row, Self], GeneralRegressionIIDModel[Row, Self]) => DenseVector[Double])
  extends ParametricIIDModel[Row, Self] {

  override def likelihood(dataset: WeightedDataset[Row, Self], parameter: DenseVector[Double]): Double = {
    dataset.getRowsWithWeightIterator.foldLeft(0.0){case(res, (row, w)) =>
      res + w * model.likelihood(row, DenseVector(regressionFunction.value(row, parameter)))
    }
  }


  override def gradLikelihood(dataset: WeightedDataset[Row, Self], parameter: DenseVector[Double]): DenseVector[Double] = {
    dataset.getRowsWithWeightIterator.foldLeft(
         DenseVector.zeros[Double](dim)
      ){case(res, (row, w)) =>
        val gradModel = model.gradLikelihood(row, DenseVector(regressionFunction.value(row, parameter)))
        assert(gradModel.length == 1)
        res + regressionFunction.grad(row, parameter) * (gradModel(0) * w)
      }
  }

  override def fisherMatrix(dataset: WeightedDataset[Row, Self]): DenseMatrix[Double] = null

  override def MLE(dataset: WeightedDataset[Row, Self]): DenseVector[Double] = optimizer(dataset, this)

}

