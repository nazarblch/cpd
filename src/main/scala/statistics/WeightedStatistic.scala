package statistics

import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import datasets._

import scala.collection.mutable.ArrayBuffer


trait WeightedStatistic[Row, Self <: Dataset[Row, Self], V] {
  def getValue(dataset: Self, weights: Vector[Double], offset: Int = 0): V
}

abstract class AdditiveWeightedStatistic[Row, Self <: Dataset[Row, Self]](dimension: Int) extends WeightedStatistic[Row, Self, Double] {
  def getValue(row: Row): Double
  def getValue(dataset: Dataset[Row, Self], weights: Vector[Double]): Double =
      WeightedDataset(dataset, weights).convolution(getValue(_))
}

class GaussianStatistic extends WeightedStatistic[Vector[Double], MultiColumnDataset[Double], Array[Double]] {
  override def getValue(dataset: MultiColumnDataset[Double], weights: Vector[Double], offset: Int): Array[Double] = {
    weights.toArray
  }
}