package cp_detectors

import bootstrap.{WeightedBootstrap, SmoothOnesGenerator, WeightedVectorBootstrap, Bootstrap}
import breeze.linalg.DenseVector
import datasets.CellT._
import datasets.Dataset
import models.ParametricModel
import patterns.TrianglePattern
import statistics.{PatternStatistic, MaxStatistic, PatternWeightedStatistic, TailStatistic}
import statistics.likelihood_ratio.{LikelihoodRatioStatistic, WeightedLikelihoodRatioStatistic}

import scala.collection.mutable.ArrayBuffer


class LRTOfflineDetector[T >: TCellDouble, P](val model: ParametricModel[T, DenseVector[Double]])
  extends OfflineChangePointDetector[T] {

  // fixme: check the same statistics for bootstrap and for CP search

  override def findAll(dataset: Dataset[T]): IndexedSeq[Int] = {

    assert(configurations.forall(_.upperBound.isDefined))

    val res: ArrayBuffer[Int] = ArrayBuffer()

    val cp = findMaxCPSignalIndex(dataset)

    if (cp.isDefined) {
      res += cp.get
      val (data1, data2) = dataset.splitAt(cp.get)
      if (data1.size > 2 * windowSizes.min) res ++= findAll(data1)
      if (data2.size > 2 * windowSizes.min) res ++= findAll(data2)
    }

    res.toVector.sorted
  }

  class Config (val windowSize: Int,
                val windowWeight: Double,
                var upperBound: Option[Double],
                var maxDist: Option[TailStatistic]){}

  val windowSizes = Array(10, 20, 100)
  val windowWeights = Array(1.0, 0.5, 0.25)
  val SAMPLE_SIZE = 10
  val CONFIDENCE = 0.05
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

    for (config <- configurations) {
      config.upperBound = Some(config.maxDist.get.quantile(CONFIDENCE))
    }

  }

  override def init(dataset: Dataset[T]): Unit = {

    for (config <- configurations) {
      setMaxDist(config, dataset)
    }

    setUpperBounds(dataset)

  }

  def findMaxCPSignalIndex(dataset: Dataset[T]): Option[Int] = {

    val cp_candidates: Array[Boolean] = Array.fill[Boolean](dataset.size)(false)
    val ratings: Array[Double] = Array.fill[Double](dataset.size)(0D)

    for (config <- configurations.filter(_.windowSize < dataset.size / 2)) {
      // fixme: prevent statistics recalculation
      val lrt = new LikelihoodRatioStatistic[T, Dataset[T]](model, config.windowSize)
      val pattern = new TrianglePattern(2 * config.windowSize)
      val patt_lrt = new PatternStatistic[T, Dataset[T]](pattern, lrt)
      val parr_lrt_res = patt_lrt.getValueWithLocations(dataset)

      parr_lrt_res.filter(_._2 > config.upperBound.get).foreach{case(i,v) => cp_candidates(i) = true}
      parr_lrt_res.foreach{case(i,v) => ratings(i) += v * config.windowWeight}
    }

    if (cp_candidates.forall(!_)) {
      None
    } else {
      Some(ratings.zipWithIndex.maxBy(_._1)._2)
    }

  }

  override def name: String = "LRTOfflineDetector"
}
