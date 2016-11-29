package cp_detectors

import bootstrap._
import breeze.linalg.DenseVector
import datasets.CellT._
import datasets.Dataset
import models.ParametricModel
import patterns.{PatternFactory, StaticTrianglePattern, TrianglePattern}
import statistics._
import statistics.likelihood_ratio._
import viz.utils.PlotXY

import scala.collection.mutable.ArrayBuffer


class LRTOfflineDetector[Row, Self <: Dataset[Row, Self]](val model: ParametricModel[Row, Self, DenseVector[Double]],
                                                             val CONFIDENCE: Double,
                                                             val windowSizes: Array[Int],
                                                          val patternFactory: PatternFactory)
  extends RecurrentOfflineChangePointDetector[Row, Self] {


  class Config (val windowSize: Int,
                var upperBound: Double){}

  val SAMPLE_SIZE = 1000
  var configurations: Array[Config] = _

  private def getConfig(dataset: Self, windowSize: Int): Config = {

    val stat = createStatistic(windowSize)
    val max_patt = new MaxStatistic[Row, Self](stat)

    println("calibrate h = " + windowSize)

    val bootstrap: Bootstrap[Row, Self] = new EmpiricalBootstrap[Row, Self](max_patt)
    val bound = bootstrap.quantile(1 - CONFIDENCE, dataset, SAMPLE_SIZE)

    new Config(windowSize, bound)
  }

  def init(dataset: Self): Unit = {
    configurations = windowSizes.sorted.par.map(h => getConfig(dataset, h)).toArray
  }

  def createStatistic(windowSize: Int): PatternStatistic[Row, Self] = {
    val lrt = new LikelihoodRatioStatistic[Row, Self](model, windowSize)
    val pattern = patternFactory(windowSize)
    new PatternStatistic[Row, Self](pattern, lrt)
  }

//  def createStatistic(windowSize: Int): LikelihoodRatioStatistic[Row, Self] = {
//    new LikelihoodRatioStatistic[Row, Self](model, windowSize)
//  }

  override def findOne(dataset: Self): Option[Int] = {

    for (config <- configurations.filter(_.windowSize < dataset.size / 4).sortBy(_.windowSize)) {

      println("test h = " + config.windowSize)
      val stat = createStatistic(config.windowSize)
      val (pos, score) = stat.getValueWithLocations(dataset).maxBy(_._2)
      if (score > config.upperBound) return Some(pos)
    }

    None
  }

  override def name: String = "LRTOffline"

  override def minWindowSize: Int = windowSizes.min
}
