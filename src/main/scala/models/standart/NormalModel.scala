package models.standart

import breeze.linalg.{inv, DenseMatrix, DenseVector}
import datasets.{OneColumnDataset, MultiColumnDataset, WeightedDataset, DataHeader}
import models.ParametricIIDModel
import breeze.stats.distributions.{Gaussian, Dirichlet}

class NormalModelVec(override val dim: Int) extends ParametricIIDModel[Vector[Double], MultiColumnDataset[Double]] {

  val V = DenseMatrix.eye[Double](dim)
  val Vinv: DenseMatrix[Double] = inv(V)

  override def likelihood(dataRow: Vector[Double], parameter: DenseVector[Double]): Double = {
    assert(parameter.length == dim)

    val m = parameter
    val x = DenseVector(dataRow.toArray)

    (x - m).t * Vinv * (x - m) * (- 0.5)
  }

  override def fisherMatrix(dataset: WeightedDataset[Vector[Double], MultiColumnDataset[Double]]): DenseMatrix[Double] = null

  override def gradLikelihood(dataRow: Vector[Double], parameter: DenseVector[Double]): DenseVector[Double] = {

    assert(parameter.length == dim)

    val m = parameter
    val x = DenseVector(dataRow.toArray)

    val grad_m: DenseVector[Double] =  Vinv * (m - x) * (-1.0)

    grad_m
  }

  override def MLE(dataset: WeightedDataset[Vector[Double], MultiColumnDataset[Double]]): DenseVector[Double] = {
    val mle_m = dataset.convolutionV(row => DenseVector(row.toArray)) / dataset.weights.sum

    mle_m
  }

  override def header: DataHeader = DataHeader(dim)
}



class NormalModel extends ParametricIIDModel[Double, OneColumnDataset[Double]] {

  override val dim: Int = 2
  val minV = 0.1

  override def likelihood(dataRow: Double, parameter: DenseVector[Double]): Double = {
    assert(parameter.length == dim)

    val m = parameter(0)
    var v = parameter(1)
    val x = dataRow
    if (v < minV) v = minV

    - 0.5 * math.log(2 * math.Pi * v) - 0.5 * math.pow(x - m, 2) / v
  }

  override def fisherMatrix(dataset: WeightedDataset[Double, OneColumnDataset[Double]]): DenseMatrix[Double] = {
    val mle = MLE(dataset.toDataset)
    val m = mle(0)
    val v = mle(1)
    DenseMatrix(Array(1/v, 0.0), Array(0.0, 0.5/(v*v))) * dataset.weights.sum
  }

  override def gradLikelihood(dataRow: Double, parameter: DenseVector[Double]): DenseVector[Double] = {

    assert(parameter.length == dim)

    val m = parameter(0)
    var v = parameter(1)
    val x = dataRow

    assert(v > minV - 1e-8)

    val grad_m: Double = -(m - x) / v
    val grad_v: Double = - 0.5 / v + 0.5 * math.pow(x - m, 2) / (v*v)

    DenseVector(grad_m, grad_v)
  }

  override def MLE(dataset: WeightedDataset[Double, OneColumnDataset[Double]]): DenseVector[Double] = {
    val mle_m = dataset.convolution(x => x) / dataset.weights.sum
    var mle_v = dataset.convolution(x => math.pow(x - mle_m, 2)) / math.abs(dataset.weights.sum)
    if (mle_v < minV) {
      mle_v = minV
    }

    DenseVector(mle_m, mle_v)
  }

  override def header: DataHeader = DataHeader(1)
}


class NormalModelMean(val v: Double = 2) extends ParametricIIDModel[Double, OneColumnDataset[Double]] {

  override val dim: Int = 1

  override def likelihood(dataRow: Double, parameter: DenseVector[Double]): Double = {
    assert(parameter.length == dim)

    val m = parameter(0)
    val x = dataRow

    - 0.5 * math.log(2 * math.Pi * v) - 0.5 * math.pow(x - m, 2) / v
  }

  override def fisherMatrix(dataset: WeightedDataset[Double, OneColumnDataset[Double]]): DenseMatrix[Double] = DenseMatrix(dataset.weights.sum/v)

  override def gradLikelihood(dataRow: Double, parameter: DenseVector[Double]): DenseVector[Double] = {

    assert(parameter.length == dim)

    val m = parameter(0)
    val x = dataRow

    val grad_m: Double = -(m - x) / v

    DenseVector(grad_m)
  }

  override def MLE(dataset: WeightedDataset[Double, OneColumnDataset[Double]]): DenseVector[Double] = {
    val mle_m = dataset.convolution(x => x) / dataset.weights.sum

    DenseVector(mle_m)
  }

  override def header: DataHeader = DataHeader(1)
}
