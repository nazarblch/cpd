package statistics

import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import datasets._

import scala.collection.mutable.ArrayBuffer


trait WeightedStatistic[T >:  DoubleCellT with IntCellT with CatCellT with Double, V] {
  def getValue(dataset: Dataset[T], weights: Vector[Double], offset: Int = 0): V
}

abstract class AdditiveWeightedStatistic[T >:  DoubleCellT with IntCellT with CatCellT with Double](dimension: Int) extends WeightedStatistic[T, Double] {
  def getValue(row: IndexedSeq[T]): Double
  def getValue(dataset: Dataset[T], weights: Vector[Double]): Double =
      WeightedDataset(dataset, weights).convolution(getValue(_))
}

class GaussianStatistic extends WeightedStatistic[Double, Array[Double]] {
  override def getValue(dataset: Dataset[Double], weights: Vector[Double], offset: Int): Array[Double] = {
    weights.toArray
  }
}