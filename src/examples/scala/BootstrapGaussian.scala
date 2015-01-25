package examples.scala

import bootstrap.{SmoothOnesGenerator, WeightedVectorBootstrap, Bootstrap}
import datasets.{DatasetLoader, DatasetConverter, Dataset}
import statistics.GaussianStatistic

/**
 * Created by nazar on 1/18/15.
 */
object BootstrapGaussian extends App {

  val bootstrap: Bootstrap[Double] = new WeightedVectorBootstrap[Double](new SmoothOnesGenerator, new GaussianStatistic)

  val data1: Dataset[Double] = DatasetConverter.toNumeric(DatasetLoader.loadFromFile("data/numeric.csv"))

  println(bootstrap.mean(data1, 100))

  println(bootstrap.variance(data1, 100))


  println(bootstrap.quantile(0.977, data1, 10000))


}
