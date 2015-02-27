package cp_detectors

import bootstrap.{WeightedBootstrap, SmoothOnesGenerator, WeightedVectorBootstrap, Bootstrap}
import breeze.linalg.DenseVector
import datasets.CellT._
import datasets.Dataset
import models.ParametricModel
import patterns.TrianglePattern
import statistics.{PatternStatistic, MaxStatistic, PatternWeightedStatistic, TailStatistic}
import statistics.likelihood_ratio.{MeanVarWeightedLikelihoodRatioStatistic, LikelihoodRatioStatistic, WeightedLikelihoodRatioStatistic}
import viz.utils.PlotXY

import scala.collection.mutable.ArrayBuffer


class LRTOfflineDetector[T >: TCellDouble, P](val model: ParametricModel[T, DenseVector[Double]], val CONFIDENCE: Double = 0.03)
  extends OfflineChangePointDetector[T] {

  // fixme: check the same statistics for bootstrap and for CP search

  override def findAll(dataset: Dataset[T]): IndexedSeq[Int] = {

    calibrate(dataset.subset(0, 100))

    assert(configurations.filter(_.windowSize < dataset.size / 4.0).forall(_.upperBound.isDefined))

    val res: ArrayBuffer[Int] = ArrayBuffer()

    val cp = findMaxCPSignalIndex(dataset)

    if (cp.isDefined) {
      res += cp.get
      val (data1, data2) = dataset.splitAt(cp.get)
      if (data1.size > 4 * windowSizes.min) res ++= new LRTOfflineDetector[T, P](model, CONFIDENCE ).findAll(data1)
      if (data2.size > 4 * windowSizes.min) res ++= new LRTOfflineDetector[T, P](model, CONFIDENCE ).findAll(data2)
    }

    res.toVector.sorted
  }

  class Config (val windowSize: Int,
                val windowWeight: Double,
                var upperBound: Option[Double],
                var maxDist: Option[TailStatistic]){}

  val windowSizes = Array(10, 15, 20)
  val windowWeights = Array(3.0, 1.0, 0.5)
  val SAMPLE_SIZE = 100
  val configurations: Array[Config] = windowSizes.zip(windowWeights).map{case(h, w) => new Config(h, w, None, None)}

  private def setMaxDist(config: Config, dataset: Dataset[T]): Unit = {

    val wlrt = new WeightedLikelihoodRatioStatistic[T](model, config.windowSize)
    val pattern = new TrianglePattern(2 * config.windowSize)
    val patt_wlrt = new PatternWeightedStatistic[T](pattern, wlrt)
    val max_patt_wlrt = new MaxStatistic[T](patt_wlrt)

    val bootstrap: Bootstrap[T] = new WeightedBootstrap[T](new SmoothOnesGenerator, max_patt_wlrt)

    val sample = bootstrap.sample(dataset, SAMPLE_SIZE)

    config.maxDist = Some(new TailStatistic(sample.data))
  }

  private def setUpperBounds(dataset: Dataset[T]): Unit = {

    for (config <- configurations.filter(_.maxDist.isDefined)) {
      config.upperBound = Some(config.maxDist.get.quantile(CONFIDENCE))
    }

  }

  def init(dataset: Dataset[T]): Unit = {

  }

  def calibrate(dataset: Dataset[T]): Unit = {

    for (config <- configurations.filter(_.windowSize < dataset.size / 4.0)) {
      setMaxDist(config, dataset)
    }

    setUpperBounds(dataset)

  }

  def findMaxCPSignalIndex(dataset: Dataset[T]): Option[Int] = {

    val cp_candidates: Array[Boolean] = Array.fill[Boolean](dataset.size)(false)
    val ratings: Array[Double] = Array.fill[Double](dataset.size)(0D)

    for (config <- configurations.filter(_.windowSize < dataset.size / 4.0)) {
      // fixme: prevent statistics recalculation
      val lrt = new LikelihoodRatioStatistic[T, Dataset[T]](model, config.windowSize)
      val pattern = new TrianglePattern(2 * config.windowSize)
      val patt_lrt = new PatternStatistic[T, Dataset[T]](pattern, lrt)
      val parr_lrt_res = patt_lrt.getValueWithLocations(dataset)

      parr_lrt_res.filter(_._2 > config.upperBound.get).foreach{case(i,v) => {
        cp_candidates(i) = true
        ratings(i) += v * config.windowWeight / config.windowSize
      }}




//      val pl = new PlotXY("t", "LRTs")
//      pl.addline(parr_lrt_res, "patt_lrt")
//      pl.addline(Array.fill[Double](parr_lrt_res.length)(config.upperBound.get), "boot")
//      pl.print("test.png")
    }

    if (cp_candidates.forall(!_)) {
      None
    } else {
      Some(ratings.zipWithIndex.maxBy(_._1)._2)
    }

  }

  override def name: String = "LRTOfflineDetector"
}
