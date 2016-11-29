package examples

import cp_detectors._
import datasets.OneColumnDataset
import detector_test_system.{TestDataLoader, TestsManager}
import models.standart.NormalModel
import patterns.{StaticTrianglePattern, TrianglePattern}

object TestOfflineDetector extends App {

  val tests = TestDataLoader.loadOfflineTestConfigurations("testdataoffline/config.xml", "N", Set("NMI"))

  println(tests.length)

  val manager = new TestsManager[Double, OneColumnDataset[Double], OfflineChangePointDetector[Double, OneColumnDataset[Double]]](tests, "testdataoffline_res")

  //val detector = new MeanVarOfflineDetector
  // val detector = new OnlineAdapter[Double](new CUSUMOfflineDetector, 10, 5)
  //val detector = new LRTOnlineDetector("Volatility")
  //detector.init(Dataset.applyVec(Gaussian(2,10).sample(100).map(x => DenseVector(x))))

 //val detector = new BOffCPD


  val model =  new NormalModel()
  val detector = new LRTOfflineDetector(model, 0.1, Array(70), StaticTrianglePattern)
//
   manager.testDetector(detector, true)
//
  println("NMI \t" + manager.getPlot("NMI", detector.name, "delta"))



}