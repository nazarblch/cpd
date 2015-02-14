package statistics

import datasets.CellT._
import datasets.Dataset

class PValueStatistic[T >: TCellDouble](val dist: TailStatistic, val statistic: WeightedStatistic[T, Double])
  extends WeightedStatistic[T, Double] {

  override def getValue(dataset: Dataset[T], weights: Vector[Double], offset: Int): Double = {
    dist.getPValue(statistic.getValue(dataset, weights))
  }

}