package statistics

import breeze.linalg.DenseVector
import datasets.CellT._
import datasets.Dataset
import patterns.{PatternMatcher, CurvePattern}
import statistics.likelihood_ratio.LikelihoodRatioStatistic


class PatternWeightedStatistic[T >: TCellDouble](val pattern: CurvePattern, val statistic: WeightedStatistic[T, Array[Double]])
  extends WeightedStatistic[T, Array[Double]] {

  def getValue(stats: Array[Double]): Array[Double] = {
    PatternMatcher.convolution(pattern, DenseVector(stats))
  }

  override def getValue(dataset: Dataset[T], weights: Vector[Double], offset: Int): Array[Double] = {
    val stats = statistic.getValue(dataset, weights)
    assert(!stats.exists(_.isNaN))
    getValue(stats)
  }

}

class PatternStatistic[T >: TCellDouble, D <: Dataset[T]](val pattern: CurvePattern, val statistic: LikelihoodRatioStatistic[T, D]) {

  def getValue(stats: Array[(Int, Double)]): Array[Double] = {
    PatternMatcher.convolution(pattern, DenseVector(stats.map(_._2)))
  }

  def getLocations(stats: Array[(Int, Double)]): Array[Int] = {
    // assert(pattern.getXSize % 2 == 0)
    val h = pattern.getXSize / 2
    stats.map(_._1).slice(h, stats.length - h)
  }

  def getValueWithLocations(dataset: D): Array[(Int, Double)] = {
    val stats = statistic.getValueWithLocations(dataset)
    getLocations(stats).zip(getValue(stats))
  }


}



