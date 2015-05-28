import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import cp_detectors._
import datasets.{OneColumnDataset, Dataset}
import models.standart.{NormalModel, NormalModelMean}
import quality.QualityMeasures
import statistics.likelihood_ratio.{ExtendedLikelihoodRatioStatistic, MeanVarWeightedLikelihoodRatioStatistic}

object TestOfflineDetector extends App {

  val tests = TestDataLoader.loadOfflineTestConfigurations("testdataoffline/config.xml", "N", Set("NMI"))

  println(tests.length)

  val manager = new TestsManager[Double, OneColumnDataset[Double], OfflineChangePointDetector[Double, OneColumnDataset[Double]]](tests, "testdataoffline_res")

  //val detector = new MeanVarOfflineDetector
  // val detector = new OnlineAdapter[Double](new CUSUMOfflineDetector, 10, 5)
  //val detector = new LRTOnlineDetector("Volatility")
  //detector.init(Dataset.applyVec(Gaussian(2,10).sample(100).map(x => DenseVector(x))))

 //val detector = new BOffCPD


  //val model =  new NormalModelMean(2)
  val model =  new NormalModel
  val detector = new LRTOfflineDetector[Double, OneColumnDataset[Double], DenseVector[Double]](model, 0.1, MeanVarWeightedLikelihoodRatioStatistic)

  manager.testDetector(detector, false)

  println("NMI \t" + manager.getPlot("NMI", detector.name, "delta"))



}