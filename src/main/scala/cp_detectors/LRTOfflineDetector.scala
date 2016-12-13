package cp_detectors

import bootstrap._
import breeze.linalg.DenseVector
import datasets.Dataset
import models.ParametricModel
import patterns.PatternFactory
import statistics._
import statistics.likelihood_ratio._
import viz.utils.PlotXY


class LRTOfflineDetector[Row, DatasetType <: Dataset[Row, DatasetType]](val model: ParametricModel[Row, DatasetType, DenseVector[Double]],
                                                             val CONFIDENCE: Double,
                                                             val windowSizes: Array[Int],
                                                          val patternFactory: PatternFactory)
  extends RecurrentOfflineChangePointDetector[Row, DatasetType] {


  class Config (val windowSize: Int,
                var upperBound: Double){}

  val SAMPLE_SIZE = 1000
  var configurations: Array[Config] = _

  private def getConfig(dataset: DatasetType, windowSize: Int): Config = {

    val stat = createStatistic(windowSize)
    val max_patt = new MaxStatistic[Row, DatasetType](stat)

    val bootstrap: Bootstrap[Row, DatasetType] = new EmpiricalBootstrap[Row, DatasetType](max_patt)
    val bound = bootstrap.quantile(1 - CONFIDENCE, dataset, SAMPLE_SIZE)

    new Config(windowSize, bound)
  }

  def init(dataset: DatasetType): Unit = {
    configurations = windowSizes.sorted.par.map(h => getConfig(dataset, h)).toArray
  }

  def createStatistic(windowSize: Int): PatternStatistic[Row, DatasetType] = {
    val lrt = new LikelihoodRatioStatistic[Row, DatasetType](model, windowSize)
    val pattern = patternFactory(windowSize)
    new PatternStatistic[Row, DatasetType](pattern, lrt)
  }

  def plot(dataset: DatasetType) : Unit = {
    for ((windowSize, stat) <- windowSizes zip windowSizes.map(createStatistic).map(_.getValueWithLocations(dataset))) {
      val pl = new PlotXY("t", "stat value ")
      pl.addline(stat, "stat window="  + windowSize)
    }
  }

  override def findOne(dataset: DatasetType): Option[Int] = {
    configurations.filter(_.windowSize < dataset.size / 4).sortBy(_.windowSize).toStream.flatMap {config =>
      val stat = createStatistic(config.windowSize)
      val (pos, score) = stat.getValueWithLocations(dataset).maxBy(_._2)
      if (score > config.upperBound) Some(pos) else None
    } headOption
  }

  override def name: String = "LRTOffline"

  override def minWindowSize: Int = windowSizes.min
}
