import cp_detectors.{OfflineChangePointDetector, OnlineChangePointDetector}
import datasets.CellT._
import datasets.Dataset
import quality.QualityMeasure


class TestElement[T >: TCellDouble] (val data: Dataset[T], val reference: IndexedSeq[Int]){}


abstract class DetectorTester[T >: TCellDouble, D](
                                                    testData: Seq[TestElement[T]],
                                                    val measures: Set[QualityMeasure[T]]) {

  def testElement(element: TestElement[T], detector: D): Unit

  def run(detector: D) {
    testData.foreach(elem => testElement(elem, detector))
  }

  def getScore: Set[(String, Double)] = measures.map(measure => (measure.name, measure.getScore))

}


class OnlineDetectorTester[T >: TCellDouble](testData: Seq[TestElement[T]], measures: Set[QualityMeasure[T]])
    extends DetectorTester[T, OnlineChangePointDetector[T]](testData, measures) {

  override def testElement(element: TestElement[T], detector: OnlineChangePointDetector[T]) {
    
    val dataset: Dataset[T] = element.data
    val firstCP: Int = if (element.reference.length == 0) dataset.size else element.reference(0)
    detector.init(dataset.subset(0, firstCP))

    val new_cp_signals = dataset.getRowsIterator.map(row => {
      detector.addData(row)
      detector.hasNewChangePoint
    }).zipWithIndex.filter(_._1).map(_._2)
    
    measures.foreach(_.addObservation(new_cp_signals, element.reference, dataset))
    
  }

}


class OfflineDetectorTester[T >: TCellDouble](testData: Seq[TestElement[T]], measures: Set[QualityMeasure[T]])
    extends DetectorTester[T, OfflineChangePointDetector[T]](testData, measures) {

  override def testElement(element: TestElement[T], detector: OfflineChangePointDetector[T]) {

    val dataset: Dataset[T] = element.data
    detector.init(dataset)

    val predictions = detector.findAll

    measures.foreach(_.addObservation(predictions, element.reference, dataset))

  }

}
