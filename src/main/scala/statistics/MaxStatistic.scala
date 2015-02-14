package statistics

import datasets.CellT._
import datasets.Dataset

// fixme: make max extrapolation for big data sets

class MaxStatistic[T >: TCellDouble](val statistic: WeightedStatistic[T, Array[Double]])
  extends WeightedStatistic[T, Double] {

  override def getValue(dataset: Dataset[T], weights: Vector[Double], offset: Int): Double = {
    statistic.getValue(dataset, weights).max
  }

}


class AggregatedMinStatistic[T >: TCellDouble](val statistics: Seq[WeightedStatistic[T, Double]])
  extends WeightedStatistic[T, Double] {

  override def getValue(dataset: Dataset[T], weights: Vector[Double], offset: Int): Double = {
    statistics.map(_.getValue(dataset, weights, offset)).min
  }

}