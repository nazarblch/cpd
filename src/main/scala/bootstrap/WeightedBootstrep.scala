package bootstrap

import breeze.linalg.DenseVector
import datasets.{Dataset, CatCellT, IntCellT, DoubleCellT}
import statistics.WeightedStatistic

import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting


trait Bootstrap[T >:  DoubleCellT with IntCellT with CatCellT with Double] {
  def sample(dataset: Dataset[T], sampleSize: Int): DenseVector[Double]

  def mean(dataset: Dataset[T], sampleSize: Int): Double = breeze.stats.mean(sample(dataset, sampleSize))

  def variance(dataset: Dataset[T], sampleSize: Int): Double = breeze.stats.variance(sample(dataset, sampleSize))

  def quantile(leqPr: Double, dataset: Dataset[T], sampleSize: Int): Double = {
    val orderStatistic: Int = (sampleSize * leqPr).toInt
    val data: Array[Double] = sample(dataset, sampleSize).toArray
    Sorting.quickSort(data)
    data(orderStatistic)
  }
}

class WeightedBootstrap[T >:  DoubleCellT with IntCellT with CatCellT with Double](val weightsGenerator: SmoothOnesGenerator,
                                                                                   val statistic: WeightedStatistic[T, Double]) extends Bootstrap[T] {

  def sample(dataset: Dataset[T], sampleSize: Int): DenseVector[Double] = {
    DenseVector.fill[Double](sampleSize)(statistic.getValue(dataset, weightsGenerator.generateVector(dataset.size)))
  }
}

class WeightedVectorBootstrap[T >:  DoubleCellT with IntCellT with CatCellT with Double](val weightsGenerator: SmoothOnesGenerator,
                                                                                         val statistic: WeightedStatistic[T, Array[Double]]) extends Bootstrap[T] {

  def sample(dataset: Dataset[T], sampleSize: Int): DenseVector[Double] = {

    val res: ArrayBuffer[Double] = ArrayBuffer.apply[Double]()

    while (res.length < sampleSize) {
      res ++= statistic.getValue(dataset, weightsGenerator.generateVector(dataset.size))
    }

    DenseVector[Double](res.toArray.slice(0, sampleSize))
  }
}
