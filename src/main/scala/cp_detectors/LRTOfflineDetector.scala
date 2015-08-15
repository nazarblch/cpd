package cp_detectors

import bootstrap.{WeightedBootstrap, SmoothOnesGenerator, WeightedVectorBootstrap, Bootstrap}
import breeze.linalg.DenseVector
import datasets.CellT._
import datasets.Dataset
import models.ParametricModel
import patterns.{StaticTrianglePattern, TrianglePattern}
import statistics.{PatternStatistic, MaxWeightedStatistic, PatternWeightedStatistic, TailStatistic}
import statistics.likelihood_ratio._
import viz.utils.PlotXY

import scala.collection.mutable.ArrayBuffer


class LRTOfflineDetector[Row, Self <: Dataset[Row, Self], P](val model: ParametricModel[Row, Self, DenseVector[Double]],
                                                             val CONFIDENCE: Double = 0.03,
                                                             val wstatFactory: WeightedStatisticFactory)
  extends OfflineChangePointDetector[Row, Self] {

  // fixme: check the same statistics for bootstrap and for CP search

  override def findAll(dataset: Self): IndexedSeq[Int] = {

    calibrate(dataset)

    assert(configurations.filter(_.windowSize < dataset.size / 4.0).forall(_.upperBound.isDefined))

    val res: ArrayBuffer[Int] = ArrayBuffer()

    val cp = findMaxCPSignalIndex(dataset)

    if (cp.isDefined) {
      res += cp.get
      val data1 = dataset.subset(0, cp.get + windowSizes.min)
      val data2 = dataset.subset(cp.get - windowSizes.min, dataset.size)

      if (data1.size > 4 * windowSizes.min) res ++= new LRTOfflineDetector[Row, Self, P](model, CONFIDENCE/2, wstatFactory).findAll(data1)
      if (data2.size > 4 * windowSizes.min) res ++=
          new LRTOfflineDetector[Row, Self, P](model, CONFIDENCE/2, wstatFactory).findAll(data2).map(p => p + cp.get - windowSizes.min)
    }

    res.toVector.sorted
  }

  class Config (val windowSize: Int,
                val windowWeight: Double,
                var upperBound: Option[Double],
                var maxDist: Option[TailStatistic]){}

  val windowSizes = Array(30, 50, 70)
  val windowWeights = Array(3.0, 2.0, 1.0)
  val SAMPLE_SIZE = 1000
  val configurations: Array[Config] = windowSizes.zip(windowWeights).map{case(h, w) => new Config(h, w, None, None)}

  private def setMaxDist(config: Config, dataset: Self): Unit = {

    val wlrt = wstatFactory(model, config.windowSize)
    val pattern = new TrianglePattern(2 * config.windowSize)
    val patt_wlrt = new PatternWeightedStatistic[Row, Self](pattern, wlrt)
    val max_patt_wlrt = new MaxWeightedStatistic[Row, Self](patt_wlrt)

    val bootstrap: Bootstrap[Row, Self] = new WeightedBootstrap[Row, Self](new SmoothOnesGenerator, max_patt_wlrt)

    val sample = bootstrap.sample(dataset, SAMPLE_SIZE)

    config.maxDist = Some(new TailStatistic(sample.data))
  }

  private def setUpperBounds(dataset: Self): Unit = {

    for (config <- configurations.filter(_.maxDist.isDefined)) {
      config.upperBound = Some(config.maxDist.get.quantile(CONFIDENCE))
    }

  }

  def init(dataset: Self): Unit = {

  }

  def calibrate(dataset: Self): Unit = {

    for (config <- configurations.filter(_.windowSize < dataset.size / 4.0)) {
      setMaxDist(config, dataset)
    }

    setUpperBounds(dataset)

  }

  def findMaxCPSignalIndex(dataset: Self): Option[Int] = {

    val cp_candidates: Array[Boolean] = Array.fill[Boolean](dataset.size)(false)
    val ratings: Array[Double] = Array.fill[Double](dataset.size)(0D)

    for (config <- configurations.filter(_.windowSize < dataset.size / 4.0)) {
      // fixme: prevent statistics recalculation
      val lrt = new LikelihoodRatioStatistic[Row, Self](model, config.windowSize)
      val pattern = new TrianglePattern(2 * config.windowSize)
      val patt_lrt = new PatternStatistic[Row, Self](pattern, lrt)
      val parr_lrt_res = patt_lrt.getValueWithLocations(dataset)

      parr_lrt_res.filter(_._2 > config.upperBound.get).foreach{case(i,v) => {
        cp_candidates(i) = true
        ratings(i) += v * config.windowWeight / config.windowSize
      }}

//      parr_lrt_res.filter(_._2 < config.upperBound.get).foreach{case(i,v) => {
//        cp_candidates(i) = false
//      }}

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

  override def name: String = "LRTOffline"
}
