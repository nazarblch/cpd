package models.standart

import breeze.linalg.{DenseMatrix, DenseVector}
import datasets.{WeightedDataset, DataHeader}
import models.ParametricIIDModel

/**
 * Created by nazar on 1/18/15.
 */
class NormalModel extends ParametricIIDModel[Double, DenseVector[Double]] {
  override def dim(parameter: DenseVector[Double]): Int = 1

  override def likelihood(dataRow: IndexedSeq[Double], parameter: DenseVector[Double]): Double = - 0.5 * math.log(2 * math.Pi) - 0.5 * math.pow(dataRow(0) - parameter(0), 2)

  override def fisherMatrix(dataset: WeightedDataset[Double]): DenseMatrix[Double] = null

  override def gradLikelihood(dataRow: IndexedSeq[Double], parameter: DenseVector[Double]): DenseVector[Double] = DenseVector(Array( -(parameter(0) - dataRow(0))))

  override def MLE(dataset: WeightedDataset[Double]): DenseVector[Double] =
    DenseVector(Array( dataset.getRowsWithWeightIterator.foldLeft(0.0)({case(res, (row, w)) => res + w * row(0) }) / dataset.weights.sum))

  override def header: DataHeader = new DataHeader(IndexedSeq("double"))
}
