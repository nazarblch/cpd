package models.standart

import breeze.linalg.{inv, DenseMatrix, DenseVector}
import datasets.{WeightedDataset, DataHeader}
import models.ParametricIIDModel
import breeze.stats.distributions.{Gaussian, Dirichlet}

class NormalModelVec(override val dim: Int) extends ParametricIIDModel[Double] {

  val V = DenseMatrix.eye[Double](dim)
  val Vinv: DenseMatrix[Double] = inv(V)

  override def likelihood(dataRow: IndexedSeq[Double], parameter: DenseVector[Double]): Double = {
    assert(parameter.length == dim)

    val m = parameter
    val x = DenseVector(dataRow.toArray)

    (x - m).t * Vinv * (x - m) * (- 0.5)
  }

  override def fisherMatrix(dataset: WeightedDataset[Double]): DenseMatrix[Double] = null

  override def gradLikelihood(dataRow: IndexedSeq[Double], parameter: DenseVector[Double]): DenseVector[Double] = {

    assert(parameter.length == dim)

    val m = parameter
    val x = DenseVector(dataRow.toArray)

    val grad_m: DenseVector[Double] =  Vinv * (m - x) * (-1.0)

    grad_m
  }

  override def MLE(dataset: WeightedDataset[Double]): DenseVector[Double] = {
    val mle_m = dataset.getRowsWithWeightIterator.
        foldLeft(DenseVector.zeros[Double](dim)) { case (res, (row, w)) => res + DenseVector(row.toArray) * w} / dataset.weights.sum

    mle_m
  }

  override def header: DataHeader = DataHeader(dim)
}



class NormalModel extends ParametricIIDModel[Double] {

  override val dim: Int = 2
  val minV = 0.5

  override def likelihood(dataRow: IndexedSeq[Double], parameter: DenseVector[Double]): Double = {
    assert(parameter.length == dim)
    assert(dataRow.length == 1)

    val m = parameter(0)
    var v = parameter(1)
    val x = dataRow(0)
    if (v < minV) v = minV

    - 0.5 * math.log(2 * math.Pi * v) - 0.5 * math.pow(x - m, 2) / v
  }

  override def fisherMatrix(dataset: WeightedDataset[Double]): DenseMatrix[Double] = null

  override def gradLikelihood(dataRow: IndexedSeq[Double], parameter: DenseVector[Double]): DenseVector[Double] = {

    assert(parameter.length == dim)
    assert(dataRow.length == 1)

    val m = parameter(0)
    var v = parameter(1)
    val x = dataRow(0)

    val grad_m: Double = -(m - x) / v
    val grad_v: Double = - 0.5 / v + 0.5 * math.pow(x - m, 2) / (v*v)

    DenseVector(grad_m, grad_v)
  }

  override def MLE(dataset: WeightedDataset[Double]): DenseVector[Double] = {
    val mle_m = dataset.getRowsWithWeightIterator.foldLeft(0.0) { case (res, (row, w)) => res + w * row(0)} / dataset.weights.sum
    var mle_v = dataset.getRowsWithWeightIterator.
        foldLeft(0.0) { case (res, (row, w)) => res + w * math.pow(row(0) - mle_m, 2)} / math.abs(dataset.weights.sum)
    if (mle_v < minV) {
      mle_v = minV
    }

    DenseVector(mle_m, mle_v)
  }

  override def header: DataHeader = DataHeader(1)
}


class NormalModelMean(val v: Double = 2) extends ParametricIIDModel[Double] {

  override val dim: Int = 1

  override def likelihood(dataRow: IndexedSeq[Double], parameter: DenseVector[Double]): Double = {
    assert(parameter.length == dim)
    assert(dataRow.length == 1)

    val m = parameter(0)
    val x = dataRow(0)

    - 0.5 * math.log(2 * math.Pi * v) - 0.5 * math.pow(x - m, 2) / v
  }

  override def fisherMatrix(dataset: WeightedDataset[Double]): DenseMatrix[Double] = null

  override def gradLikelihood(dataRow: IndexedSeq[Double], parameter: DenseVector[Double]): DenseVector[Double] = {

    assert(parameter.length == dim)
    assert(dataRow.length == 1)

    val m = parameter(0)
    val x = dataRow(0)

    val grad_m: Double = -(m - x) / v

    DenseVector(grad_m)
  }

  override def MLE(dataset: WeightedDataset[Double]): DenseVector[Double] = {
    val mle_m = dataset.getRowsWithWeightIterator.foldLeft(0.0) { case (res, (row, w)) => res + w * row(0)} / dataset.weights.sum

    DenseVector(mle_m)
  }

  override def header: DataHeader = DataHeader(1)
}
