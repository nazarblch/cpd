import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import cp_detectors.{LRTOfflineDetector, OnlineAdapter, MeanVarOfflineDetector}
import datasets.{OneColumnDataset, Dataset}
import likelihood_optimize.AdaptiveGradientDescentOptimizer
import models.standart.{NormalModelVec, NormalModelMean, NormalModel}
import patterns.TrianglePattern
import statistics.likelihood_ratio.{ExtendedLikelihoodRatioStatistic, MeanVarWeightedLikelihoodRatioStatistic, LikelihoodRatioStatistic}


object LRTOfflineTest extends App {


  val data = Gaussian(2,1.0).sample(150) ++ Gaussian(2.0,1.0).sample(150)

  val model =  new NormalModel

  val time = System.currentTimeMillis()

  val detector = new LRTOfflineDetector[Double, OneColumnDataset[Double], DenseVector[Double]](model, 0.1, MeanVarWeightedLikelihoodRatioStatistic)

  println(detector.findAll(Dataset.apply(data)).mkString(","))

  println((System.currentTimeMillis() - time) / 1000.0)


}
