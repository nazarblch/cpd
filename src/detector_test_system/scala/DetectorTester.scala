import cp_detectors.{OfflineChangePointDetector, OnlineChangePointDetector}
import datasets.CellT._
import datasets.Dataset
import quality.{QualityMeasures, QualityMeasure}


class TestElement[T >: TCellDouble] (val data: Dataset[T], val reference: IndexedSeq[Int]){}


abstract class DetectorTester[T >: TCellDouble, D](
                                                    testData: Seq[TestElement[T]],
                                                    val measureNames: Set[String]) {

  var measures: Set[QualityMeasure[T]] = QualityMeasures.apply[T](measureNames)

  def testElement(element: TestElement[T], detector: D): Unit

  def run(detector: D) {
    testData.foreach(elem => testElement(elem, detector))
  }

  def getScore: Set[(String, Option[Double])] = {
    val res =  measures.map(measure => (measure.name, measure.getScore))
    measures = QualityMeasures.apply[T](measureNames)
    res
  }

}


class OnlineDetectorTester[T >: TCellDouble](testData: Seq[TestElement[T]], measureNames: Set[String])
    extends DetectorTester[T, OnlineChangePointDetector[T]](testData, measureNames) {

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


class OfflineDetectorTester[T >: TCellDouble](testData: Seq[TestElement[T]],  measureNames: Set[String])
    extends DetectorTester[T, OfflineChangePointDetector[T]](testData, measureNames) {

  override def testElement(element: TestElement[T], detector: OfflineChangePointDetector[T]) {

    val dataset: Dataset[T] = element.data
    detector.init(dataset)

    val predictions = detector.findAll

    measures.foreach(_.addObservation(predictions, element.reference, dataset))

  }

}
