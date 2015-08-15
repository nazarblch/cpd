package statistics

import breeze.linalg.DenseVector
import datasets.CellT._
import datasets.{OneColumnDataset, Dataset}
import models.{ParametricModel, ParametricIIDModel}
import patterns.{StaticTrianglePattern, TrianglePattern, PatternMatcher, CurvePattern}
import statistics.likelihood_ratio.{WeightedStatisticFactory, LikelihoodRatioStatistic}


class PatternWeightedStatistic[Row, Self <: Dataset[Row, Self]](val pattern: CurvePattern, val statistic: WeightedStatistic[Row, Self, Array[Double]])
  extends WeightedStatistic[Row, Self, Array[Double]] {

  def getValue(stats: Array[Double]): Array[Double] = {
    PatternMatcher.convolution(pattern, DenseVector(stats))
  }

  override def getValue(dataset: Self, weights: Vector[Double], offset: Int): Array[Double] = {
    val stats = statistic.getValue(dataset, weights)
    assert(!stats.exists(_.isNaN))
    val wstat = getValue(stats)

    //val lrt = statistic.getValue(dataset, Vector.fill(dataset.size)(1.0))
    //val conv = getValue(lrt)

    //wstat.zip(conv).map{case(x,y) => math.abs(x-y)}
    wstat
  }

}

class PatternStatistic[Row, Self <: Dataset[Row, Self]](val pattern: CurvePattern, val statistic: LikelihoodRatioStatistic[Row, Self])
  extends Statistic[Row, Self, Array[Double]] {

  def getValue(stats: Array[(Int, Double)]): Array[Double] = {
    PatternMatcher.convolution(pattern, DenseVector(stats.map(_._2)))
  }

  def getLocations(stats: Array[(Int, Double)]): Array[Int] = {
    // assert(pattern.getXSize % 2 == 0)
    val h = pattern.getXSize / 2
    stats.map(_._1).slice(h, stats.length - h)
  }

  def getValueWithLocations(dataset: Self): Array[(Int, Double)] = {
    val stats = statistic.getValueWithLocations(dataset)
    getLocations(stats).zip(getValue(stats))
  }

  override def getValue(dataset: Self, offset: Int): Array[Double] = getValueWithLocations(dataset).map(_._2)
}

object PatternStatistic {
  def apply[Row, Self <: Dataset[Row, Self]](model: ParametricModel[Row, Self, DenseVector[Double]], h: Int): PatternStatistic[Row, Self] = {

    val stat = new LikelihoodRatioStatistic(model, h)
    val pattern = new TrianglePattern(2 * h)
    new PatternStatistic(pattern, stat)

  }
}






