package bootstrap

import breeze.linalg.DenseVector
import datasets.{CatCellT, Dataset, DoubleCellT, IntCellT}
import statistics.{Statistic, WeightedStatistic}

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.mutable.ParArray
import scala.util.{Random, Sorting}


trait Bootstrap[Row, Self <: Dataset[Row, Self]] {

  def mean(dataset: Self, sampleSize: Int): Double = breeze.stats.mean(sample(dataset, sampleSize))

  def variance(dataset: Self, sampleSize: Int): Double = breeze.stats.variance(sample(dataset, sampleSize))

  def quantile(leqPr: Double, dataset: Self, sampleSize: Int): Double = {
    val orderStatistic: Int = (sampleSize * leqPr).toInt
    val data: Array[Double] = sample(dataset, sampleSize).toArray
    Sorting.quickSort(data)
    data(orderStatistic)
  }

  def sampleOne(dataset: Self): Double

  def sample(dataset: Self, sampleSize: Int): DenseVector[Double] = {
    val data: Array[Double] = ParArray.range(0, sampleSize).map(i => sampleOne(dataset)).toArray
    DenseVector(data)
  }
}

class RefDistBootstrap[Row, Self <: Dataset[Row, Self]](val dataGenerator: Double => Self,
                                                         val statistic: Statistic[Row, Self, Double]) extends Bootstrap[Row, Self] {

  override def sampleOne(dataset: Self): Double = statistic.getValue(dataGenerator.apply(0.0))
}

class WeightedBootstrap[Row, Self <: Dataset[Row, Self]](val weightsGenerator: SmoothOnesGenerator,
                                                         val statistic: WeightedStatistic[Row, Self, Double]) extends Bootstrap[Row, Self] {


  override def sampleOne(dataset: Self): Double = statistic.getValue(dataset, weightsGenerator.generateVector(dataset.size))

}

class EmpiricalBootstrap[Row, Self <: Dataset[Row, Self]](val statistic: Statistic[Row, Self, Double]) extends Bootstrap[Row, Self] {

  val r = new Random()

  private def genIndexes(size: Int): Array[Int] = Array.fill(size)(r.nextInt(size))

  override def sampleOne(dataset: Self): Double = statistic.getValue(dataset.subset(genIndexes(dataset.size)))

}


