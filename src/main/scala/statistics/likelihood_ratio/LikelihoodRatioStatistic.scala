package statistics.likelihood_ratio

import breeze.linalg._
import breeze.optimize._
import datasets.CellT._
import datasets._
import models.standart.NormalModel
import models.{ParametricModel, ParametricIIDModel, Model}
import statistics.{WeightedStatistic}
import utils.sqrt

import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.mutable.ParArray


class LikelihoodRatioStatistic[Row, Self <: Dataset[Row, Self]](val model: Model[Row, Self], override val windowSize: Int)
   extends  WindowStatistic[Row, Self](windowSize){

  def getValue(windowIndex: Int, dataset: Self): Double = {
    val  (data1, data2) = getWindowData(windowIndex, dataset)

    val L1 = model.likelihood(data1)
    val L2 = model.likelihood(data2)
    val L = model.likelihood(data1 ++ data2)

    val res = math.sqrt(2.0) * math.sqrt(L1 + L2 - L + 1e-5)
    if (res.isNaN) {

      println("NAN !!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      return 0
      //throw new Exception("NAN statistic value")
    }
    res
  }

}


abstract class WindowStatistic[Row, Self <: Dataset[Row, Self]](val windowSize: Int) {


  def windowCount(dataset: Dataset[Row, Self]): Int = {
    assert(dataset.size > 2 * windowSize)
    dataset.size - 2 * windowSize
  }

  def getWindowCoordinate(windowIndex: Int): (Int, Int, Int) = {
    val left: Int = windowIndex
    val middle: Int = windowIndex + windowSize
    val right: Int = middle + windowSize
    (left, middle, right)
  }

  def getWindowMiddles(dataset: Dataset[Row, Self]): Array[Int] = {
    val wc: Int = windowCount(dataset)
    Array.range(0, wc).map(windowIndex => getWindowCoordinate(windowIndex)._2)
  }

  def getWindowLefts(dataset: Dataset[Row, Self]): Array[Int] = {
    val wc: Int = windowCount(dataset)
    Array.range(0, wc).map(windowIndex => getWindowCoordinate(windowIndex)._1)
  }

  def getWindowRights(dataset: Dataset[Row, Self]): Array[Int] = {
    val wc: Int = windowCount(dataset)
    Array.range(0, wc).map(windowIndex => getWindowCoordinate(windowIndex)._3)
  }


  def getWindowData(windowIndex: Int, dataset: Self): (Self, Self) = {
    val (left, middle, right) = getWindowCoordinate(windowIndex)
    val data1: Self = dataset.subset(left, middle)
    val data2: Self = dataset.subset(middle, right)
    (data1, data2)
  }

  def getValue(windowIndex: Int, dataset: Self): Double

  def getValue(dataset: Self): Array[Double] = {
    val wc: Int = windowCount(dataset)

    ParArray.range(0, wc).map(windowIndex => getValue(windowIndex, dataset)).toArray
  }

  def getValueWithLocations(dataset: Self): Array[(Int, Double)] = {
    getWindowMiddles(dataset).zip(getValue(dataset))
  }

  def getValueWithLeftLocations(dataset: Self): Array[(Int, Double)] = {
    getWindowLefts(dataset).zip(getValue(dataset))
  }

  def getValueWithRightLocations(dataset: Self): Array[(Int, Double)] = {
    getWindowRights(dataset).zip(getValue(dataset))
  }

  def getValueSync(dataset: Self): Array[Double] = {
    val wc: Int = windowCount(dataset)

    Array.range(0, wc).map(windowIndex => getValue(windowIndex, dataset))
  }

}


object ExtendedLikelihoodRatioStatistic extends WeightedStatisticFactory {
  override def apply[Row, Self <: Dataset[Row, Self]](model: ParametricModel[Row, Self, DenseVector[Double]],
                                                      windowSize: Int): WeightedStatistic[Row, Self, Array[Double]] = {
    new ExtendedLikelihoodRatioStatistic(model, windowSize)
  }
}

class ExtendedLikelihoodRatioStatistic[Row, Self <: Dataset[Row, Self]](
                                                                         val model: ParametricModel[Row, Self, DenseVector[Double]],
                                                                         override val windowSize: Int)
    extends WindowStatistic[Row, WeightedDataset[Row, Self]](windowSize) with WeightedStatistic[Row, Self, Array[Double]] {



  def getXi1Xi2(windowIndex: Int, dataset: WeightedDataset[Row, Self], param1: DenseVector[Double], param2: DenseVector[Double]): (DenseVector[Double], DenseVector[Double]) = {

    val (data1, data2) = getWindowData(windowIndex, dataset)
    val D1: DenseMatrix[Double] = sqrt(model.fisherMatrix(data1))
    val D2: DenseMatrix[Double] = sqrt(model.fisherMatrix(data2))

    val gL1: DenseVector[Double] = model.gradLikelihood(data1, param1)
    val gL2: DenseVector[Double] = model.gradLikelihood(data2, param2)

    (inv(D1) * gL1, inv(D2) * gL2)
  }

  def getXi1Xi2(windowIndex: Int, dataset: WeightedDataset[Row, Self]): (DenseVector[Double], DenseVector[Double]) = {

    val (data1, data2) = getWindowData(windowIndex, dataset)
    getXi1Xi2(windowIndex, dataset, model.MLE(data1.toDataset), model.MLE(data2.toDataset))
  }


  private def getSigma(windowIndex: Int, dataset: WeightedDataset[Row, Self]): DenseMatrix[Double] = {
    val (data1, data2) = getWindowData(windowIndex, dataset)
    val D12: DenseMatrix[Double] = model.fisherMatrix(data1)
    val D22: DenseMatrix[Double] = model.fisherMatrix(data2)

    val D2 = D12 + D22

    val S2: DenseMatrix[Double] = D12 * inv(D2) * D22

    sqrt(S2)
  }

  def get_window_delta_xi(windowIndex: Int, dataset: WeightedDataset[Row, Self]): DenseVector[Double] = {

    val (data1, data2) = getWindowData(windowIndex, dataset)
    val D1: DenseMatrix[Double] = sqrt(model.fisherMatrix(data1))
    val D2: DenseMatrix[Double] = sqrt(model.fisherMatrix(data2))

    val xi = getXi1Xi2(windowIndex, dataset)
    val Sigma = getSigma(windowIndex, dataset)

    Sigma * (inv(D2) * xi._2 - inv(D1) * xi._1)
  }

  def getBias(windowIndex: Int, dataset: WeightedDataset[Row, Self]): DenseVector[Double] = {
    val (data1, data2) = getWindowData(windowIndex, dataset)
    val Sigma = getSigma(windowIndex, dataset)
    Sigma * (model.MLE(data2.toDataset) -  model.MLE(data1.toDataset))
  }

  def get_delta_xi_norm(windowIndex: Int, dataset: WeightedDataset[Row, Self]): Double = {

    val xi = get_window_delta_xi(windowIndex, dataset)

    math.sqrt(xi dot xi)
  }

  def get_delta_xi_norm_with_bias(windowIndex: Int, dataset: WeightedDataset[Row, Self]): Double = {

    val xi = get_window_delta_xi(windowIndex, dataset)
    val b = getBias(windowIndex, dataset)
    val s = xi + b

    math.sqrt(s dot s)
  }

  def get_delta_xi_norm(dataset: WeightedDataset[Row, Self]): Array[Double] = {
    val wc: Int = windowCount(dataset)
    ParArray.range(0, wc).map(windowIndex => get_delta_xi_norm(windowIndex, dataset)).toArray
  }



  override def getValue(windowIndex: Int, dataset: WeightedDataset[Row, Self]): Double = {
    get_delta_xi_norm(windowIndex, dataset)
  }

  override def getValue(dataset: Self, weights: scala.Vector[Double], offset: Int): Array[Double] = {
    getValue(WeightedDataset(dataset, weights))
  }


}


abstract class WeightedStatisticFactory {
  def apply[Row, Self <: Dataset[Row, Self]](model: ParametricModel[Row, Self, DenseVector[Double]], windowSize: Int): WeightedStatistic[Row, Self, Array[Double]]
}


object WeightedLikelihoodRatioStatistic extends WeightedStatisticFactory {
  override def apply[Row, Self <: Dataset[Row, Self]](model: ParametricModel[Row, Self, DenseVector[Double]],
                                                      windowSize: Int): WeightedStatistic[Row, Self, Array[Double]] = {
    new WeightedLikelihoodRatioStatistic(model, windowSize)
  }
}


class WeightedLikelihoodRatioStatistic[Row, Self <: Dataset[Row, Self]](
                                                                         val model: ParametricModel[Row, Self, DenseVector[Double]],
                                                                         override val windowSize: Int)
    extends WindowStatistic[Row, WeightedDataset[Row, Self]](windowSize) with WeightedStatistic[Row, Self, Array[Double]] {

  val ones: scala.Vector[Double] = scala.Vector.fill(windowSize)(1.0)

  override def getValue(windowIndex: Int, dataset: WeightedDataset[Row, Self]): Double = {
    val  data: (WeightedDataset[Row, Self], WeightedDataset[Row, Self]) = getWindowData(windowIndex, dataset)

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

    val lbfgs = new LBFGS[DenseVector[Double]](maxIter=50, m=3)
    val MLEH0: DenseVector[Double] = lbfgs.minimize(LH0, MLELeft)
    val L = - LH0.calculate(MLEH0)._1

    val L1 = model.likelihood(data._1)
    val L2 = model.likelihood(data._2)
    // val L = model.likelihood(data._1 ++ data._2)


    val res = math.sqrt(2.0) * math.sqrt(L1 + L2 - L + 1e-5)
    if (res.isNaN) {
      throw new Exception("NAN statistic value")
    }
    res
  }

  override def getValue(dataset: Self, weights: scala.Vector[Double], offset: Int): Array[Double] = {
    getValue(WeightedDataset(dataset, weights))
  }

}


object MeanVarWeightedLikelihoodRatioStatistic extends WeightedStatisticFactory {
  override def apply[Row, Self <: Dataset[Row, Self]](model: ParametricModel[Row, Self, DenseVector[Double]], windowSize: Int): WeightedStatistic[Row, Self, Array[Double]] = {
    new MeanVarWeightedLikelihoodRatioStatistic(model.asInstanceOf[NormalModel], windowSize).asInstanceOf[WeightedStatistic[Row, Self, Array[Double]]]
  }
}

class MeanVarWeightedLikelihoodRatioStatistic(val model: NormalModel,
                                              override val windowSize: Int)
  extends WindowStatistic[Double, WeightedDataset[Double, OneColumnDataset[Double]]](windowSize) with WeightedStatistic[Double, OneColumnDataset[Double], Array[Double]] {

  val ones: scala.Vector[Double] = scala.Vector.fill(windowSize)(1.0)

  def findMLEH0(data: (WeightedDataset[Double, OneColumnDataset[Double]], WeightedDataset[Double, OneColumnDataset[Double]]),
                MLELeft: DenseVector[Double], MLERight: DenseVector[Double]): (DenseVector[Double], DenseVector[Double]) = {

    val deltaMLE: DenseVector[Double] = MLERight - MLELeft

    val LH0 = new DiffFunction[DenseVector[Double]] {
      def calculate(m: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val v = (MLELeft(1) + MLERight(1)) / 2
        val x1: DenseVector[Double] = DenseVector(m(0), v)
        val x2: DenseVector[Double] = DenseVector(m(0) + deltaMLE(0), v)
        (
          -model.likelihood(data._1, x1) - model.likelihood(data._2, x2),
          DenseVector(-model.gradLikelihood(data._1, x1)(0) - model.gradLikelihood(data._2, x2)(0))
          )
      }
    }

    val lbfgs = new LBFGS[DenseVector[Double]](maxIter=100, m=3)
    val m: Double = lbfgs.minimize(LH0, DenseVector(MLELeft(0)))(0)

    val LH0_v = new DiffFunction[DenseVector[Double]] {
      def calculate(v: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val x1: DenseVector[Double] = DenseVector(m, math.max(v(0), model.minV))
        val x2: DenseVector[Double] = DenseVector(m + deltaMLE(0), math.max(v(0) + deltaMLE(1), model.minV))
        (
          -model.likelihood(data._1, x1) - model.likelihood(data._2, x2),
          DenseVector(-model.gradLikelihood(data._1, x1)(1) - model.gradLikelihood(data._2, x2)(1))
          )
      }
    }

    val lbfgs_v = new LBFGS[DenseVector[Double]](maxIter=100, m=3)
    val v: Double = lbfgs_v.minimize(LH0_v, DenseVector(MLELeft(1)))(0)

    val mle1 = DenseVector(m, math.max(v, model.minV))
    val mle2 = DenseVector(m + deltaMLE(0), math.max(v + deltaMLE(1), model.minV))

    (mle1, mle2)
  }

  override def getValue(windowIndex: Int, dataset: WeightedDataset[Double, OneColumnDataset[Double]]): Double = {
    val  data: (WeightedDataset[Double, OneColumnDataset[Double]], WeightedDataset[Double, OneColumnDataset[Double]]) = getWindowData(windowIndex, dataset)

    val MLELeft: DenseVector[Double]  = model.MLE(data._1.toDataset, ones)
    val MLERight: DenseVector[Double] = model.MLE(data._2.toDataset, ones)

    val (mle1, mle2) = findMLEH0(data, MLELeft, MLERight)

    val L = model.likelihood(data._1, mle1) + model.likelihood(data._2, mle2)

    val L1 = model.likelihood(data._1)
    val L2 = model.likelihood(data._2)
    //val L12 = model.likelihood(data._1 ++ data._2)


    val res = math.sqrt(2.0) * math.sqrt(L1 + L2 - L + 1e-5)
    if (res.isNaN) {
      throw new Exception("NAN statistic value")
    }

    res
  }

  override def getValue(dataset: OneColumnDataset[Double], weights: scala.Vector[Double], offset: Int): Array[Double] = {
    getValue(WeightedDataset(dataset, weights))
  }

}




class SimpleWeightedLikelihoodRatioStatistic[Row, Self <: Dataset[Row, Self]](
                                                                         val model: ParametricModel[Row, Self, DenseVector[Double]],
                                                                         override val windowSize: Int)
  extends WindowStatistic[Row, WeightedDataset[Row, Self]](windowSize) with WeightedStatistic[Row, Self, Array[Double]] {


  override def getValue(windowIndex: Int, dataset: WeightedDataset[Row, Self]): Double = {
    val  data: (WeightedDataset[Row, Self], WeightedDataset[Row, Self]) = getWindowData(windowIndex, dataset)

    val L1 = model.likelihood(data._1)
    val L2 = model.likelihood(data._2)
    val L = model.likelihood(data._1 ++ data._2)

    val res = math.sqrt(2.0) * math.sqrt(L1 + L2 - L)
    if (res.isNaN) {
      throw new Exception("NAN statistic value")
    }

    res
  }

  override def getValue(dataset: Self, weights: scala.Vector[Double], offset: Int): Array[Double] = {
    getValue(WeightedDataset(dataset, weights))
  }

}





