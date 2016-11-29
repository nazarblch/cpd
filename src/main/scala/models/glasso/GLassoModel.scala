package models.glasso

import breeze.linalg._
import breeze.numerics._
import breeze.math._
import datasets.{MultiColumnDataset, WeightedDataset, DataHeader}
import models.ParametricModel

class GLassoModel(val oneDim: Int,
                  override val header: DataHeader,
                  val regularization: Double = 0.1) extends ParametricModel[scala.Vector[Double], MultiColumnDataset[Double], DenseVector[Double]] {

  override def dim = oneDim * oneDim

  def getCovarianceMatrix(dataset: WeightedDataset[scala.Vector[Double], MultiColumnDataset[Double]]): DenseMatrix[Double] = {

    val X0: DenseVector[Double] = dataset.getRowsWithWeightIterator.foldLeft(DenseVector.zeros[Double](oneDim))({case(res, (row, w)) => {
      val X: DenseVector[Double] = DenseVector(row.toArray)
      res + X * w
    }}) / dataset.weights.sum

    dataset.getRowsWithWeightIterator.foldLeft(DenseMatrix.zeros[Double](oneDim, oneDim))({case(res, (row, w)) => {
      val X: DenseVector[Double] = DenseVector(row.toArray) - X0
      res + X * X.t * w
    }}) / (dataset.weights.sum)
  }

  def toMatrix(parameter: DenseVector[Double]): DenseMatrix[Double] = {
    parameter.toDenseMatrix.reshape(oneDim, oneDim)
  }

  def toVector(parameter: DenseMatrix[Double]): DenseVector[Double] = {
    parameter.toDenseVector
  }

  def Reg(parameter: DenseMatrix[Double]): Double = {
    var res: Double = 0

    for (i <- 0 until oneDim; j <- 0 until oneDim) {
      if (i != j) res = res + abs(parameter(i, j))
    }

    res
  }

  def gradReg(parameter: DenseMatrix[Double]): DenseMatrix[Double] = {
    val res: DenseMatrix[Double] = DenseMatrix.zeros(oneDim, oneDim)

    for (i <- 0 until oneDim; j <- 0 until oneDim) {
      if (i != j && abs(parameter(i, j)) > 1e-5) res(i, j) = signum(parameter(i, j))
    }

    res
  }

  override def likelihood(dataset: WeightedDataset[scala.Vector[Double], MultiColumnDataset[Double]], parameter: DenseVector[Double]): Double = {
    val cov = getCovarianceMatrix(dataset)
    val InvCov: DenseMatrix[Double] = toMatrix(parameter)

    dataset.weights.sum * ( log(det(InvCov)) - trace(cov * InvCov) - regularization * Reg(InvCov) )
  }

  override def fisherMatrix(dataset: WeightedDataset[scala.Vector[Double], MultiColumnDataset[Double]]): DenseMatrix[Double] = null

  override def gradLikelihood(dataset: WeightedDataset[scala.Vector[Double], MultiColumnDataset[Double]], parameter: DenseVector[Double]): DenseVector[Double] = {
    val InvCov: DenseMatrix[Double] = toMatrix(parameter)
    val res = inv(InvCov) - getCovarianceMatrix(dataset) - gradReg(InvCov) * regularization

    toVector(res) * dataset.weights.sum
  }

  override def MLE(dataset: WeightedDataset[scala.Vector[Double], MultiColumnDataset[Double]]): DenseVector[Double] = {
    val data = dataset.toDataset.getRowsIterator.map(_.toArray).toArray
    val res = GLassoInverse.apply(data)

    DenseVector(res.flatten)
  }


}
