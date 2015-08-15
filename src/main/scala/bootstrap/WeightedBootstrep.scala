package bootstrap

import breeze.linalg.DenseVector
import datasets.{Dataset, CatCellT, IntCellT, DoubleCellT}
import statistics.{WeightedStatistic, Statistic}

import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting


trait Bootstrap[Row, Self <: Dataset[Row, Self]] {
  def sample(dataset: Self, sampleSize: Int): DenseVector[Double]

  def mean(dataset: Self, sampleSize: Int): Double = breeze.stats.mean(sample(dataset, sampleSize))

  def variance(dataset: Self, sampleSize: Int): Double = breeze.stats.variance(sample(dataset, sampleSize))

  def quantile(leqPr: Double, dataset: Self, sampleSize: Int): Double = {
    val orderStatistic: Int = (sampleSize * leqPr).toInt
    val data: Array[Double] = sample(dataset, sampleSize).toArray
    Sorting.quickSort(data)
    data(orderStatistic)
  }
}

class RefDistBootstrap[Row, Self <: Dataset[Row, Self]](val dataGenerator: Double => Self,
                                                         val statistic: Statistic[Row, Self, Double]) {

  def sample(sampleSize: Int): DenseVector[Double] = {
    DenseVector.fill[Double](sampleSize)(statistic.getValue(dataGenerator.apply(0.0)))
  }

}

class WeightedBootstrap[Row, Self <: Dataset[Row, Self]](val weightsGenerator: SmoothOnesGenerator,
                                                         val statistic: WeightedStatistic[Row, Self, Double]) extends Bootstrap[Row, Self] {

  def sample(dataset: Self, sampleSize: Int): DenseVector[Double] = {
    DenseVector.fill[Double](sampleSize)(statistic.getValue(dataset, weightsGenerator.generateVector(dataset.size)))
  }

}

class WeightedVectorBootstrap[Row, Self <: Dataset[Row, Self]](val weightsGenerator: SmoothOnesGenerator,
                                                               val statistic: WeightedStatistic[Row, Self, Array[Double]]) extends Bootstrap[Row, Self] {

  def sample(dataset: Self, sampleSize: Int): DenseVector[Double] = {

    val res: ArrayBuffer[Double] = ArrayBuffer.apply[Double]()

    while (res.length < sampleSize) {
      res ++= statistic.getValue(dataset, weightsGenerator.generateVector(dataset.size))
    }

    DenseVector[Double](res.toArray.slice(0, sampleSize))
  }
}
