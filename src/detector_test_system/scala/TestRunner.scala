import cp_detectors.ChangePointDetector
import datasets.CellT._

import scala.collection.mutable

class TestConfiguration[T >: TCellDouble, D] (val tester: DetectorTester[T, D],
                                              val params: Map[String, Double]) {

}

class TestResult (val params: Map[String, Double], val value: Double, val measureName: String) {}

class TestsManager[T >: TCellDouble, D <: ChangePointDetector[T]](val tests: Seq[TestConfiguration[T, D]]) {

  val history: mutable.HashMap[String, Seq[TestResult]] = _

  def runTest(conf: TestConfiguration[T, D], detector: D): Unit = {
    conf.tester.run(detector)
    val res = conf.tester.getScore.toSeq.map({case (measure, value) => new TestResult(detector.params ++ conf.params, value, measure)})

    history.update(detector.name, history.getOrElse(detector.name, Seq()) ++ res)
  }

  def getPlot(measureName: String, detectorName: String, parameterName: String): Seq[(Double,Double)] = {
    history.get(detectorName).get.filter(tr => tr.measureName == measureName).map(tr => (tr.params.get(parameterName).get, tr.value))
  }

}


object TestRunner {




}
