
import datasets.OneColumnDataset
import detector_test_system.quality.{Precision, Recall}
import org.scalatest._

class PrecisionRecallSpec extends FlatSpec {

  "A precision and recall" should "return None when no experiments are provided" in {
    val precision: Precision[Double, OneColumnDataset[Double]] = new Precision[Double, OneColumnDataset[Double]]()
    val recall: Recall[Double, OneColumnDataset[Double]] = new Recall[Double, OneColumnDataset[Double]]()
    assert(precision.getScore equals None)
    assert(recall.getScore equals None)
  }

  it should "return correct values when data is zeros" in {
    val precision: Precision[Double, OneColumnDataset[Double]] = new Precision[Double, OneColumnDataset[Double]]()
    val recall: Recall[Double, OneColumnDataset[Double]] = new Recall[Double, OneColumnDataset[Double]]()
    precision.addObservation(IndexedSeq(), IndexedSeq(), null)
    recall.addObservation(IndexedSeq(), IndexedSeq(1,2,3), null)
    assert( math.abs(precision.getScore.get - 1.0 ) < 1e-5)
    assert( math.abs(recall.getScore.get - 0.0 ) < 1e-5)
  }

  it should "return correct score values" in {
    val precision: Precision[Double, OneColumnDataset[Double]] = new Precision[Double, OneColumnDataset[Double]]()
    val recall: Recall[Double, OneColumnDataset[Double]] = new Recall[Double, OneColumnDataset[Double]]()
    precision.addObservation(IndexedSeq(1,2,3,4,5,6,7), IndexedSeq(1,2,3,4,8), null)
    precision.addObservation(IndexedSeq(5), IndexedSeq(4,8), null)
    recall.addObservation(IndexedSeq(1,5,6,7), IndexedSeq(3,5,7), null)
    recall.addObservation(IndexedSeq(6,7), IndexedSeq(3,8), null)
    assert( math.abs(precision.getScore.get - 5.0/8 ) < 1e-5)
    assert( math.abs(recall.getScore.get - 3.0/5 ) < 1e-5)
  }


}
