import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import cp_detectors._
import datasets.Dataset
import models.standart.{NormalModel, NormalModelMean}
import quality.QualityMeasures

object TestOnlineDetector extends App {

  val tests = TestDataLoader.loadOnlineTestConfigurations("testdata/config.xml", "Po", Set("Precision", "Recall", "Delay"))

  println(tests.length)

  val manager = new TestsManager[Double, OnlineChangePointDetector[Double]](tests, "testres")

  // val detector = new OnlineAdapter[Double](new MeanVarOfflineDetector, 10, 5)
  // val detector = new OnlineAdapter[Double](new CUSUMOfflineDetector, 10, 5)
  //val detector = new LRTOnlineDetector("Volatility")
  //detector.init(Dataset.applyVec(Gaussian(2,10).sample(100).map(x => DenseVector(x))))

 // val detector = new D1BayesianOnlineCPDetector

  val detector = new LRTOnlineDetector(new NormalModel)
  detector.init(Dataset.applyVec(Gaussian(1,0.8).sample(100).map(x => DenseVector(x))))

  manager.testDetector(detector, false)

  println("Delay \t" + manager.getPlot("Delay", detector.name, "delta"))

  println("Precision \t" + manager.getPlot("Precision", detector.name, "delta"))

  println("Recall \t" + manager.getPlot("Recall", detector.name, "delta"))

}
