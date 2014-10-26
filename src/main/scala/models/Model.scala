package models

import breeze.linalg.{DenseMatrix, DenseVector}
import datasets.CellT.CellType
import datasets.{WeightedDataset, DataHeader}


class ModelParameter(val value: DenseVector[Double]) {
  def size: Int = value.length
}


trait Model[T >: CellType with Double, P] {

  def header: DataHeader

  def computeLikelihood(dataset: WeightedDataset[T], parameter: P): Double

  def likelihood(dataset: WeightedDataset[T], parameter: P): Double = {
    assert(header equals dataset.header)
    computeLikelihood(dataset, parameter)
  }

  def MLE(dataset: WeightedDataset[T]): P

  def likelihood(dataset: WeightedDataset[T]): Double = {
    likelihood(dataset, MLE(dataset))
  }

  def gradLikelihood(dataset: WeightedDataset[T], parameter: P): DenseVector[Double]

  def fisherMatrix(dataset: WeightedDataset[T]): DenseMatrix[Double]

}

trait ParametricModel[T >: CellType with Double, P] extends Model[T,P] {

  def computeLikelihood(dataRow: IndexedSeq[T], parameter: P): Double

  def dim(parameter: P): Int

  def gradLikelihood(dataRow: IndexedSeq[T], parameter: P): DenseVector[Double]

  override def computeLikelihood(dataset: WeightedDataset[T], parameter: P): Double = {
    dataset.getRowsWithWeightIterator.foldLeft(0.0){case(res, (row, w)) =>
      res + w * computeLikelihood(row, parameter)
    }
  }

  override def gradLikelihood(dataset: WeightedDataset[T], parameter: P): DenseVector[Double] = {
    dataset.getRowsWithWeightIterator.foldLeft(DenseVector.zeros[Double](dim(parameter))){case(res, (row, w)) =>
      res + gradLikelihood(row, parameter) * w
    }
  }

}


class GeneralRegressionModel[T >: CellType with Double](val header: DataHeader,
                                                        val regressionFunction: (IndexedSeq[T], ModelParameter) => Double,
                                                        val model: ParametricModel[T, Double] ) extends Model[T, ModelParameter] {

  override def computeLikelihood(dataset: WeightedDataset[T], parameter: ModelParameter): Double = {
    dataset.getRowsWithWeightIterator.foldLeft(0.0){case(res, (row, w)) =>
      res + w * model.computeLikelihood(row, regressionFunction(row, parameter))
    }
  }


  override def fisherMatrix(dataset: WeightedDataset[T]): DenseMatrix[Double] = null

  override def gradLikelihood(dataset: WeightedDataset[T], parameter: ModelParameter): DenseVector[Double] = null

  override def MLE(dataset: WeightedDataset[T]): ModelParameter = null

  
}

