import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import cp_detectors.{LRTOfflineDetector, OnlineAdapter, MeanVarOfflineDetector}
import datasets.Dataset
import likelihood_optimize.AdaptiveGradientDescentOptimizer
import models.standart.{NormalModelVec, NormalModelMean, NormalModel}
import patterns.TrianglePattern
import statistics.likelihood_ratio.LikelihoodRatioStatistic


object LRTOfflineTest extends App {


  val data = Gaussian(2,1).sample(30) ++ Gaussian(5,1).sample(30)

  val model =  new NormalModel

  val detector = new LRTOfflineDetector[Double, DenseVector[Double]](model, 0.1)



  println(detector.findAll(Dataset.applyD(data)).mkString(","))




}
