package statistics

import datasets.CellT._
import datasets.Dataset

// fixme: make max extrapolation for big data sets

class MaxWeightedStatistic[Row, Self <: Dataset[Row, Self]](val statistic: WeightedStatistic[Row, Self, Array[Double]])
  extends WeightedStatistic[Row, Self, Double] {

  override def getValue(dataset: Self, weights: Vector[Double], offset: Int): Double = {
    val res = statistic.getValue(dataset, weights)
    assert(!res.exists(_.isNaN))
    res.max
  }

}

class MaxStatistic[Row, Self <: Dataset[Row, Self]](val statistic: Statistic[Row, Self, Array[Double]])
  extends Statistic[Row, Self, Double] {

  override def getValue(dataset: Self, offset: Int): Double = {
    val res = statistic.getValue(dataset)
    assert(!res.exists(_.isNaN))
    res.max
  }

}


class AggregatedMinStatistic[Row, Self <: Dataset[Row, Self]](val statistics: Seq[WeightedStatistic[Row, Self, Double]])
  extends WeightedStatistic[Row, Self, Double] {

  override def getValue(dataset: Self, weights: Vector[Double], offset: Int): Double = {
    statistics.map(_.getValue(dataset, weights, offset)).min
  }

}