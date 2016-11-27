package detector_test_system

import cp_detectors.{ChangePointDetector, OfflineChangePointDetector, OnlineChangePointDetector}
import datasets.CellT._
import datasets.Dataset
import quality.{QualityMeasures, QualityMeasure}


class TestElement[Row, Self <: Dataset[Row, Self]] (val data: Self, val reference: IndexedSeq[Int]){}


abstract class DetectorTester[Row, Self <: Dataset[Row, Self], D <: ChangePointDetector[Row, Self]](
                                                    testData: Seq[TestElement[Row, Self]],
                                                    val measureNames: Set[String]) {

  // assert(testData.minBy(_.data.size).data.size == testData.maxBy(_.data.size).data.size)

  def testElement(element: TestElement[Row, Self], detector: D, measures: Set[QualityMeasure[Row, Self]]): Unit

  def run(detector: D): Set[(String, Option[Double])] = {
    val measures: Set[QualityMeasure[Row, Self]] = QualityMeasures.apply[Row, Self](measureNames)

    println("tests count = " + testData.size)

    testData.zipWithIndex.foreach{case(elem, i) => {
      testElement(elem, detector, measures)
      println("run test " + i)
    }}
    measures.map(measure => (measure.name, measure.getScore))
  }

}


class OnlineDetectorTester[Row, Self <: Dataset[Row, Self]](testData: Seq[TestElement[Row, Self]], measureNames: Set[String])
    extends DetectorTester[Row, Self, OnlineChangePointDetector[Row, Self]](testData, measureNames) {

  override def testElement(element: TestElement[Row, Self], detector: OnlineChangePointDetector[Row, Self], measures: Set[QualityMeasure[Row, Self]]) {
    
    val dataset: Self = element.data

    val firstCP =
      if (element.reference.length > 0) element.reference.min
      else dataset.size
    detector.init(dataset.subset(0, firstCP + 100))

    val signals = dataset.getRowsIterator.map(row => {
      detector.addData(row)
      detector.hasNewChangePoint
    }).zipWithIndex.filter(_._1).map(_._2).sorted

    val new_cp_signals = signals.filter(_ <= firstCP) ++ signals.filter(_ > firstCP).take(1)
    
    measures.foreach(_.addObservation(new_cp_signals, element.reference, dataset))

    detector.clear()
  }



}


class OfflineDetectorTester[Row, Self <: Dataset[Row, Self]](testData: Seq[TestElement[Row, Self]],  measureNames: Set[String])
    extends DetectorTester[Row, Self, OfflineChangePointDetector[Row, Self]](testData, measureNames) {

  override def testElement(element: TestElement[Row, Self], detector: OfflineChangePointDetector[Row, Self], measures: Set[QualityMeasure[Row, Self]]) {

    val dataset: Self = element.data
    val predictions = detector.findAll(dataset)

    measures.foreach(_.addObservation(predictions, element.reference, dataset))

  }

  override def run(detector: OfflineChangePointDetector[Row, Self]): Set[(String, Option[Double])] = {
    detector.init(testData.filter(_.reference.length == 0)(0).data)
    super.run(detector)
  }

}
