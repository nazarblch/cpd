package models

import breeze.linalg.{DenseMatrix, DenseVector}
import datasets.CellT.CellType
import datasets.{Dataset, WeightedDataset, DataHeader}


trait Model[T >: CellType with Double] {

  def header: DataHeader

  def likelihood(dataset: WeightedDataset[T]): Double

  def likelihood(dataset: Dataset[T]): Double = likelihood(WeightedDataset(dataset, Vector.fill[Double](dataset.size)(1.0)))
}


trait IIDModel[T >: CellType with Double] extends Model[T] {

  def likelihood(dataRow: IndexedSeq[T]): Double
  
  override def likelihood(dataset: WeightedDataset[T]): Double = {
    assert(header equals dataset.header)
    dataset.convolution(likelihood)
  }
}

trait ParametricModel[T >: CellType with Double, P] extends Model[T] {

  override def likelihood(dataset: WeightedDataset[T]): Double = {
    assert(header equals dataset.header)
    likelihood(dataset, MLE(dataset))
  }

  def likelihood(dataset: WeightedDataset[T], parameter: P): Double

  def dim(parameter: P): Int

  def MLE(dataset: WeightedDataset[T]): P

  def MLE(dataset: Dataset[T], weights: Vector[Double]): P = MLE(WeightedDataset(dataset, weights))

  def fisherMatrix(dataset: WeightedDataset[T]): DenseMatrix[Double]

  def gradLikelihood(dataset: WeightedDataset[T], parameter: P): DenseVector[Double]

}

trait ParametricIIDModel[T >: CellType with Double, P] extends ParametricModel[T, P] {

  def likelihood(dataRow: IndexedSeq[T], parameter: P): Double

  override def likelihood(dataset: WeightedDataset[T], parameter: P): Double = {
    dataset.convolution(likelihood(_, parameter))
  }

  def gradLikelihood(dataRow: IndexedSeq[T], parameter: P): DenseVector[Double]

  override def gradLikelihood(dataset: WeightedDataset[T], parameter: P): DenseVector[Double] = {
    dataset.getRowsWithWeightIterator.foldLeft(DenseVector.zeros[Double](dim(parameter))){case(res, (row, w)) =>
      res + gradLikelihood(row, parameter) * w
    }
  }

}


abstract class GeneralRegressionIIDModel[T >: CellType with Double, P >: Double](
                                                           val header: DataHeader,
                                                           val regressionFunction: RegressionFunction[T, P],
                                                           val model: ParametricIIDModel[T, Double],
                                                           val optimizer: (WeightedDataset[T], GeneralRegressionIIDModel[T, P]) => P)
  extends ParametricIIDModel[T, P] {

  override def likelihood(dataset: WeightedDataset[T], parameter: P): Double = {
    dataset.getRowsWithWeightIterator.foldLeft(0.0){case(res, (row, w)) =>
      res + w * model.likelihood(row, regressionFunction.value(row, parameter))
    }
  }


  override def gradLikelihood(dataset: WeightedDataset[T], parameter: P): DenseVector[Double] = {
    dataset.getRowsWithWeightIterator.foldLeft(
         DenseVector.zeros[Double](dim(parameter))
      ){case(res, (row, w)) =>
        val gradModel = model.gradLikelihood(row, regressionFunction.value(row, parameter))
        assert(gradModel.length == 1)
        res + regressionFunction.grad(row, parameter) * (gradModel(0) * w)
      }
  }

  override def fisherMatrix(dataset: WeightedDataset[T]): DenseMatrix[Double] = null

  override def MLE(dataset: WeightedDataset[T]): P = optimizer(dataset, this)

}

