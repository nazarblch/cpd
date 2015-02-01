import cp_detectors.OnlineChangePointDetector
import datasets.CellT._
import datasets.Dataset
import quality.QualityMeasure

import scala.collection.immutable.Map

class TestElement[T >: TCellDouble] (val data: Dataset[T], val configuration: Map[String, Double])


class DetectorTester[T >: TCellDouble](testData: Seq[TestElement], measure: QualityMeasure[T]) {

  def testElement(element: TestElement, detector: OnlineChangePointDetector): Double = {
    val dataset = element.data
    detector.init(dataset)

    val 

    val  dataset.getRowsIterator.map(row => {
      detector.addData(row)
      if(detector.hasNewChangePoint) 1 else 0
    })
  }

  def run(detector: OnlineChangePointDetector): Unit = {

  }

}
