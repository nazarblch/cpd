import cp_detectors.{SimpleOnlineChangePointDetector, OnlineChangePointDetector}
import quality.QualityMeasures

object TestOnlineDetector extends App {

  val tests = TestDataLoader.loadOnlineTestConfigurations("testdata/config.xml", Set("Precision", "Recall", "Delay", QualityMeasures.NMI))

  println(tests.length)

  val manager = new TestsManager[Double, OnlineChangePointDetector[Double]](tests)

  manager.testDetector(SimpleOnlineChangePointDetector)

  println("Delay \t" + manager.getPlot("Delay", "Simple", "delta"))

  println("Precision \t" + manager.getPlot("Precision", "Simple", "delta"))

  println("Recall \t" + manager.getPlot("Recall", "Simple", "delta"))

}
