package statistics

import datasets.CellT._
import datasets.Dataset

class PValueStatistic[Row, Self <: Dataset[Row, Self]](val dist: TailStatistic, val statistic: WeightedStatistic[Row, Self, Double])
  extends WeightedStatistic[Row, Self, Double] {

  override def getValue(dataset: Self, weights: Vector[Double], offset: Int): Double = {
    dist.getPValue(statistic.getValue(dataset, weights))
  }

}