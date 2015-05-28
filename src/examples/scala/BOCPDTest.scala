import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import cp_detectors.D1BayesianOnlineCPDetector
import datasets.{Dataset, Column}
import likelihood_optimize.AdaptiveGradientDescentOptimizer
import models.standart.NormalModel
import patterns.TrianglePattern
import statistics.likelihood_ratio.LikelihoodRatioStatistic


object BOCPDTest extends App {

  val data = Gaussian(2,1).sample(50) ++ Gaussian(4,1).sample(50) ++ Gaussian(2,1).sample(50)

  val detector = new D1BayesianOnlineCPDetector

  var i = 0
  for (d <- data) {
    detector.addData(d)
    println(i + ": " + detector.hasNewChangePoint + " max = " + detector.maxes(i+1))
    i += 1
  }


}
