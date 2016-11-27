package examples

import cp_detectors._
import datasets.OneColumnDataset
import detector_test_system.{TestDataLoader, TestsManager}
import models.standart.NormalModel
import statistics.likelihood_ratio.MeanVarWeightedLikelihoodRatioStatistic

object TestOnlineDetector extends App {

  val tests = TestDataLoader.loadOnlineTestConfigurations("testdataonline/config.xml", "Po", Set("Precision", "Recall", "Delay"))

  println(tests.length)

  val manager = new TestsManager[Double, OneColumnDataset[Double], OnlineChangePointDetector[Double, OneColumnDataset[Double]]](tests, "testdataonline_res")

  //val detector = new OnlineAdapter[Double, OneColumnDataset[Double]](new MeanVarOfflineDetector, 10, 5)
  // val detector = new OnlineAdapter[Double](new CUSUMOfflineDetector, 10, 5)
  //val detector = new LRTOnlineDetector("Volatility")
  //detector.init(Dataset.applyVec(Gaussian(2,10).sample(100).map(x => DenseVector(x))))

  //val detector = new D1BayesianOnlineCPDetector

  val detector = new LRTOnlineDetector(new NormalModel, MeanVarWeightedLikelihoodRatioStatistic)
  //detector.init(Dataset.apply[Double](Gaussian(1,0.8).sample(100)))

  manager.testDetector(detector, false)

  println("Delay \t" + manager.getPlot("Delay", detector.name, "delta"))

  println("Precision \t" + manager.getPlot("Precision", detector.name, "delta"))

  println("Recall \t" + manager.getPlot("Recall", detector.name, "delta"))

}
