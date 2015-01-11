package models

import breeze.linalg.{DenseMatrix, DenseVector}
import datasets.CellT.CellType
import datasets.{WeightedDataset, DataHeader}


trait Model[T >: CellType with Double] {

  def header: DataHeader

  def likelihood(dataset: WeightedDataset[T]): Double
}

trait ParametricModel[T >: CellType with Double, P] extends Model[T] {

  override def likelihood(dataset: WeightedDataset[T]): Double = {
    likelihood(dataset, MLE(dataset))
  }

  def likelihood(dataset: WeightedDataset[T], parameter: P): Double = {
    assert(header equals dataset.header)
    computeLikelihood(dataset, parameter)
  }

  def dim(parameter: P): Int

  def MLE(dataset: WeightedDataset[T]): P

  def fisherMatrix(dataset: WeightedDataset[T]): DenseMatrix[Double]

  def computeLikelihood(dataRow: IndexedSeq[T], parameter: P): Double

  def gradLikelihood(dataRow: IndexedSeq[T], parameter: P): DenseVector[Double]

  def computeLikelihood(dataset: WeightedDataset[T], parameter: P): Double = {
    dataset.getRowsWithWeightIterator.foldLeft(0.0){case(res, (row, w)) =>
      res + w * computeLikelihood(row, parameter)
    }
  }

  def gradLikelihood(dataset: WeightedDataset[T], parameter: P): DenseVector[Double] = {
    dataset.getRowsWithWeightIterator.foldLeft(DenseVector.zeros[Double](dim(parameter))){case(res, (row, w)) =>
      res + gradLikelihood(row, parameter) * w
    }
  }

}


abstract class GeneralRegressionModel[T >: CellType with Double, P >: Double](
                                                           val header: DataHeader,
                                                           val regressionFunction: RegressionFunction[T, P],
                                                           val model: ParametricModel[T, Double],
                                                           val optimizer: (WeightedDataset[T], GeneralRegressionModel[T, P]) => P)
  extends ParametricModel[T, P] {

  override def computeLikelihood(dataset: WeightedDataset[T], parameter: P): Double = {
    dataset.getRowsWithWeightIterator.foldLeft(0.0){case(res, (row, w)) =>
      res + w * model.computeLikelihood(row, regressionFunction.value(row, parameter))
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

