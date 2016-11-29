package models.standart

import breeze.linalg.{DenseMatrix, DenseVector, det, inv}
import breeze.optimize.{DiffFunction, FirstOrderMinimizer, LBFGS}
import datasets._
import models.ParametricIIDModel
import breeze.stats.distributions.{Dirichlet, Gaussian}
import models.glasso.{GLassoInverse}
import smile.regression._

class NormalModelVecMean(override val dim: Int) extends ParametricIIDModel[DenseVector[Double], DenseVectorDataset] {

  val V = DenseMatrix.eye[Double](dim)
  val Vinv: DenseMatrix[Double] = inv(V)

  override def likelihood(dataRow: DenseVector[Double], parameter: DenseVector[Double]): Double = {
    assert(parameter.length == dim)

    val m = parameter
    val x = dataRow

    (x - m).t * Vinv * (x - m) * (- 0.5)
  }

  override def fisherMatrix(dataset: WeightedDataset[DenseVector[Double], DenseVectorDataset]): DenseMatrix[Double] = null

  override def gradLikelihood(dataRow: DenseVector[Double], parameter: DenseVector[Double]): DenseVector[Double] = {

    assert(parameter.length == dim)

    val m = parameter
    val x = dataRow

    val grad_m: DenseVector[Double] =  Vinv * (m - x) * (-1.0)

    grad_m
  }

  override def MLE(dataset: WeightedDataset[DenseVector[Double], DenseVectorDataset]): DenseVector[Double] = {
    val mle_m = dataset.convolutionV(row => row) / dataset.weights.sum
    mle_m
  }

  override def header: DataHeader = DataHeader(dim)
}


class NormalModelVec(override val dim: Int) extends ParametricIIDModel[DenseVector[Double], DenseVectorDataset] {

  private def getMean(parameter: DenseVector[Double]): DenseVector[Double] = {
    parameter.slice(0, dim)
  }

  private def getVar(parameter: DenseVector[Double]): DenseMatrix[Double] = {
    val var_vec = parameter.slice(dim, parameter.length).toArray
    new DenseMatrix[Double](dim, dim, var_vec)
  }

  override def likelihood(dataRow: DenseVector[Double], parameter: DenseVector[Double]): Double = {
    assert(parameter.length == dim + dim * dim)

    val m = getMean(parameter)
    val x = DenseVector(dataRow.toArray)
    val V = getVar(parameter)

    (x - m).t * V * (x - m) * (- 0.5)
    + 0.5 * math.log(det(V))
  }

  override def fisherMatrix(dataset: WeightedDataset[DenseVector[Double], DenseVectorDataset]): DenseMatrix[Double] = null

  override def gradLikelihood(dataRow: DenseVector[Double], parameter: DenseVector[Double]): DenseVector[Double] = null

  override def MLE(dataset: WeightedDataset[DenseVector[Double], DenseVectorDataset]): DenseVector[Double] = {
    val mle_m = dataset.convolutionV(row => DenseVector(row.toArray)) / dataset.weights.sum
    val data = Array.range(0, dataset.size).map(i => dataset.getRow(i)).map(row => {
      (DenseVector(row.toArray) - mle_m).toArray
    })

    val res: Array[Array[Double]] = GLassoInverse(data)

    DenseVector(mle_m.toArray ++ res.reduce(_++_))
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


class RegressionModel(override val dim: Int, val classIndex: Int, val dataHeader: DataHeader = null) extends ParametricIIDModel[Vector[Double], MultiColumnDataset[Double]] {

  val I = DenseMatrix.eye[Double](dim)

  protected def getX(dataRow: Vector[Double]): DenseVector[Double] = {
    val x: DenseVector[Double] = DenseVector((dataRow.slice(0, classIndex) ++ dataRow.slice(classIndex + 1, dataRow.length)).toArray)
    assert(x.length == dim)
    x
  }

  protected def getXMatrix(dataset: WeightedDataset[Vector[Double], MultiColumnDataset[Double]]): Array[Array[Double]] = {
    val Xdata = dataset.dropCol(classIndex)
    Array.range(0, dataset.size).map(i => Xdata.getRow(i).toArray)
  }

  protected def getY(dataset: WeightedDataset[Vector[Double], MultiColumnDataset[Double]]): Array[Double] = {
    dataset.toDataset.data(classIndex).data.toArray
  }

  protected def getXYErr(dataRow: Vector[Double], parameter: DenseVector[Double]): (DenseVector[Double], Double, Double) = {
    val x: DenseVector[Double] = getX(dataRow)
    val y = dataRow(classIndex)
    val err: Double = (x dot parameter) - y
    (x, y, err)
  }

  override def likelihood(dataRow: Vector[Double], parameter: DenseVector[Double]): Double = {
    assert(parameter.length == dim)
    val err = getXYErr(dataRow, parameter)._3
    - 0.5 * err * err
  }

  override def fisherMatrix(dataset: WeightedDataset[Vector[Double], MultiColumnDataset[Double]]): DenseMatrix[Double] = {
    dataset.getRowsWithWeightIterator.map({case(row, w) =>
      val x: DenseVector[Double] = getX(row)
      x * x.t * w
    }).reduce(_ + _)
  }

  override def gradLikelihood(dataRow: Vector[Double], parameter: DenseVector[Double]): DenseVector[Double] = {

    assert(parameter.length == dim)

    val (x, y, err) = getXYErr(dataRow, parameter)
    val grad_m: DenseVector[Double] =  x * (-err)
    grad_m
  }

  override def MLE(dataset: WeightedDataset[Vector[Double], MultiColumnDataset[Double]]): DenseVector[Double] = {
    val F = fisherMatrix(dataset)
    val XY = dataset.convolutionV(row => {
      val x: DenseVector[Double] = getX(row)
      val y = row(classIndex)
      x * y
    })

    inv(F) * XY
  }

  override def header: DataHeader = if (dataHeader != null && dataHeader.isInstanceOf[DataHeader]) dataHeader else DataHeader(dim)
}

class RegressionModelWithL1(override val dim: Int, override val classIndex: Int, val lambda: Double, override val dataHeader: DataHeader = null) extends RegressionModel(dim, classIndex, dataHeader) {

  def getLambda(dataset: WeightedDataset[Vector[Double], MultiColumnDataset[Double]]): Double = {
    math.sqrt(dataset.size * math.log(dim + 1))
  }

  private def getPenalty(parameter: DenseVector[Double], lambda: Double): Double = {
    lambda * parameter.toArray.map(math.abs).sum
  }

  private def getPenaltyGrad(parameter: DenseVector[Double], lambda: Double): DenseVector[Double] = {
    parameter.map(pi => {
      math.signum(pi)
    }) * lambda
  }

  override def likelihood(dataset: WeightedDataset[Vector[Double], MultiColumnDataset[Double]], parameter: DenseVector[Double]): Double = {
    super.likelihood(dataset, parameter) // - getPenalty(parameter, getLambda(dataset))
  }


  override def MLE(dataset: WeightedDataset[Vector[Double], MultiColumnDataset[Double]]): DenseVector[Double] = {
    DenseVector(
    lasso(getXMatrix(dataset), getY(dataset), lambda).coefficients())

  }


}
