import cp_detectors.{ChangePointDetector, OfflineChangePointDetector, OnlineChangePointDetector}
import datasets.CellT._
import datasets.Dataset
import quality.{QualityMeasures, QualityMeasure}


class TestElement[T >: TCellDouble] (val data: Dataset[T], val reference: IndexedSeq[Int]){}


abstract class DetectorTester[T >: TCellDouble, D <: ChangePointDetector[T]](
                                                    testData: Seq[TestElement[T]],
                                                    val measureNames: Set[String]) {

  assert(testData.minBy(_.data.size).data.size == testData.maxBy(_.data.size).data.size)

  def testElement(element: TestElement[T], detector: D, measures: Set[QualityMeasure[T]]): Unit

  def run(detector: D): Set[(String, Option[Double])] = {
    val measures: Set[QualityMeasure[T]] = QualityMeasures.apply[T](measureNames)

    println("tests count = " + testData.size)

    testData.zipWithIndex.foreach{case(elem, i) => {
      testElement(elem, detector, measures)
      println("run test " + i)
    }}
    measures.map(measure => (measure.name, measure.getScore))
  }

}


class OnlineDetectorTester[T >: TCellDouble](testData: Seq[TestElement[T]], measureNames: Set[String])
    extends DetectorTester[T, OnlineChangePointDetector[T]](testData, measureNames) {

  override def testElement(element: TestElement[T], detector: OnlineChangePointDetector[T], measures: Set[QualityMeasure[T]]) {
    
    val dataset: Dataset[T] = element.data

    val new_cp_signals = dataset.getRowsIterator.map(row => {
      detector.addData(row)
      detector.hasNewChangePoint
    }).zipWithIndex.filter(_._1).map(_._2)
    
    measures.foreach(_.addObservation(new_cp_signals, element.reference, dataset))

    detector.clear()
  }

  override def run(detector: OnlineChangePointDetector[T]): Set[(String, Option[Double])] = {
    if (testData.exists(_.reference.length == 0)) {
     // detector.init(testData.filter(_.reference.length == 0)(0).data)
    } else {
      throw new Exception("WARN: homo data not found")
      val firstCP: Int = if (testData(0).reference.length == 0) testData(0).data.size else testData(0).reference(0)
      detector.init(testData(0).data.subset(0, firstCP))
    }
    super.run(detector)
  }

}


class OfflineDetectorTester[T >: TCellDouble](testData: Seq[TestElement[T]],  measureNames: Set[String])
    extends DetectorTester[T, OfflineChangePointDetector[T]](testData, measureNames) {

  override def testElement(element: TestElement[T], detector: OfflineChangePointDetector[T], measures: Set[QualityMeasure[T]]) {

    val dataset: Dataset[T] = element.data
    val predictions = detector.findAll(dataset)

    measures.foreach(_.addObservation(predictions, element.reference, dataset))

  }

  override def run(detector: OfflineChangePointDetector[T]): Set[(String, Option[Double])] = {
    detector.init(testData.filter(_.reference.length == 0)(0).data)
    super.run(detector)
  }

}
