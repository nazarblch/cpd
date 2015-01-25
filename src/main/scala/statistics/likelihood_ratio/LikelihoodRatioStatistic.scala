package statistics.likelihood_ratio

import breeze.linalg._
import breeze.optimize._
import datasets.CellT._
import datasets._
import models.{ParametricModel, ParametricIIDModel, Model}
import statistics.WeightedStatistic

import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.mutable.ParArray


class LikelihoodRatioStatistic[T >: CellType with Double, D <: Dataset[T]](val model: Model[T], val windowSize: Int) {


  def windowCount(dataset: Dataset[T]): Int = {
    assert(dataset.size > 2 * windowSize)
    dataset.size - 2 * windowSize
  }

  def getWindowCoordinate(windowIndex: Int): (Int, Int, Int) = {
    val left: Int = windowIndex
    val middle: Int = windowIndex + windowSize
    val right: Int = middle + windowSize
    (left, middle, right)
  }

  def getWindowData(windowIndex: Int, dataset: D): (D, D) = {
    val (left, middle, right) = getWindowCoordinate(windowIndex)
    val data1: D = dataset.subset(left, middle).asInstanceOf[D]
    val data2: D = dataset.subset(middle, right).asInstanceOf[D]
    (data1, data2)
  }

  def getValue(windowIndex: Int, dataset: D): Double = {
    val  (data1, data2) = getWindowData(windowIndex, dataset)

    val L1 = model.likelihood(data1)
    val L2 = model.likelihood(data2)
    val L = model.likelihood(data1 ++ data2)

    math.sqrt(L1 + L2 - L)
  }

  def getValue(dataset: D): Array[Double] = {
    val wc: Int = windowCount(dataset)

    ParArray.range(0, wc).map(windowIndex => getValue(windowIndex, dataset)).toArray
  }

  def getValueSync(dataset: D): Array[Double] = {
    val wc: Int = windowCount(dataset)

    Array.range(0, wc).map(windowIndex => getValue(windowIndex, dataset))
  }

}


class WeightedLikelihoodRatioStatistic[T >: CellType with Double](override val model: ParametricModel[T, DenseVector[Double]], override val windowSize: Int)
    extends LikelihoodRatioStatistic[T, WeightedDataset[T]](model, windowSize) with WeightedStatistic[T, Array[Double]] {

  val ones: scala.Vector[Double] = scala.Vector.fill(windowSize)(1.0)

  override def getValue(windowIndex: Int, dataset: WeightedDataset[T]): Double = {
    val  data: (WeightedDataset[T], WeightedDataset[T]) = getWindowData(windowIndex, dataset)

    val MLELeft: DenseVector[Double]  = model.MLE(data._1.toDataset, ones)
    val MLERight: DenseVector[Double] = model.MLE(data._2.toDataset, ones)
    val deltaMLE: DenseVector[Double] = MLERight - MLELeft

    val LH0 = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        (
          -model.likelihood(data._1, x) - model.likelihood(data._2, x + deltaMLE),
          model.gradLikelihood(data._1, x) * (-1.0) - model.gradLikelihood(data._2, x + deltaMLE)
        )
      }
    }

    val lbfgs = new LBFGS[DenseVector[Double]](maxIter=100, m=3)
    val MLEH0: DenseVector[Double] = lbfgs.minimize(LH0, MLELeft)
    val L = - LH0.calculate(MLEH0)._1

    val L1 = model.likelihood(data._1)
    val L2 = model.likelihood(data._2)

    math.sqrt(L1 + L2 - L)
  }

  override def getValue(dataset: Dataset[T], weights: scala.Vector[Double], offset: Int): Array[Double] = {
    super.getValue(WeightedDataset(dataset, weights))
  }

}