package models

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.optimize.FisherMatrix
import datasets.CellT.{TCellDouble, CellType}
import datasets.{Dataset, WeightedDataset, DataHeader}


trait Model[T>: TCellDouble] {

  def header: DataHeader

  def likelihood(dataset: WeightedDataset[T]): Double

  def likelihood(dataset: Dataset[T]): Double = likelihood(WeightedDataset(dataset, Vector.fill[Double](dataset.size)(1.0)))
}


trait IIDModel[T >: TCellDouble with Double] extends Model[T] {

  def likelihood(dataRow: IndexedSeq[T]): Double
  
  override def likelihood(dataset: WeightedDataset[T]): Double = {
    assert(header equals dataset.header)
    dataset.convolution(likelihood)
  }
}

trait ParametricModel[T>: TCellDouble, P] extends Model[T] {

  override def likelihood(dataset: WeightedDataset[T]): Double = {
    assert(header equals dataset.header)
    likelihood(dataset, MLE(dataset))
  }

  def likelihood(dataset: WeightedDataset[T], parameter: P): Double

  def dim: Int

  def MLE(dataset: WeightedDataset[T]): P

  def MLE(dataset: Dataset[T], weights: Vector[Double]): P = MLE(WeightedDataset(dataset, weights))

  def fisherMatrix(dataset: WeightedDataset[T]): DenseMatrix[Double]

  def gradLikelihood(dataset: WeightedDataset[T], parameter: P): P

}

trait ParametricIIDModel[T>: TCellDouble] extends ParametricModel[T, DenseVector[Double]] {

  def likelihood(dataRow: IndexedSeq[T], parameter: DenseVector[Double]): Double

  override def likelihood(dataset: WeightedDataset[T], parameter: DenseVector[Double]): Double = {
    dataset.convolution(likelihood(_, parameter))
  }

  def gradLikelihood(dataRow: IndexedSeq[T], parameter: DenseVector[Double]): DenseVector[Double]

  override def gradLikelihood(dataset: WeightedDataset[T], parameter: DenseVector[Double]): DenseVector[Double] = {
    dataset.getRowsWithWeightIterator.foldLeft(DenseVector.zeros[Double](dim)){case(res, (row, w)) =>
      res + gradLikelihood(row, parameter) * w
    }
  }

}


abstract class GeneralRegressionIIDModel[T>: TCellDouble](
                                                           val header: DataHeader,
                                                           val regressionFunction: RegressionFunction[T, DenseVector[Double]],
                                                           val model: ParametricIIDModel[T],
                                                           val optimizer: (WeightedDataset[T], GeneralRegressionIIDModel[T]) => DenseVector[Double])
  extends ParametricIIDModel[T] {

  override def likelihood(dataset: WeightedDataset[T], parameter: DenseVector[Double]): Double = {
    dataset.getRowsWithWeightIterator.foldLeft(0.0){case(res, (row, w)) =>
      res + w * model.likelihood(row, DenseVector(regressionFunction.value(row, parameter)))
    }
  }


  override def gradLikelihood(dataset: WeightedDataset[T], parameter: DenseVector[Double]): DenseVector[Double] = {
    dataset.getRowsWithWeightIterator.foldLeft(
         DenseVector.zeros[Double](dim)
      ){case(res, (row, w)) =>
        val gradModel = model.gradLikelihood(row, DenseVector(regressionFunction.value(row, parameter)))
        assert(gradModel.length == 1)
        res + regressionFunction.grad(row, parameter) * (gradModel(0) * w)
      }
  }

  override def fisherMatrix(dataset: WeightedDataset[T]): DenseMatrix[Double] = null

  override def MLE(dataset: WeightedDataset[T]): DenseVector[Double] = optimizer(dataset, this)

}

