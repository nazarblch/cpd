import cp_detectors.{SimpleOnlineChangePointDetector, OnlineChangePointDetector}

object TestOnlineDetector extends App {

  val tests = TestDataLoader.loadOnlineTestConfigurations("/Users/nazar/cpd/testdata/config.xml", Set("Precision", "Recall", "Delay"))

  println(tests.length)

  val manager = new TestsManager[Double, OnlineChangePointDetector[Double]](tests)

  manager.testDetector(SimpleOnlineChangePointDetector)

  println("Delay \t" + manager.getPlot("Delay", "Simple", "delta"))

  println("Precision \t" + manager.getPlot("Precision", "Simple", "delta"))

  println("Recall \t" + manager.getPlot("Recall", "Simple", "delta"))

}
