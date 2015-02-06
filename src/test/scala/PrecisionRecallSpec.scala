
import org.scalatest._
import quality.{Recall, Precision}

class PrecisionRecallSpec extends FlatSpec {

  "A precision and recall" should "return None when no experiments are provided" in {
    val precision: Precision[Double] = new Precision[Double]()
    val recall: Recall[Double] = new Recall[Double]()
    assert(precision.getScore equals None)
    assert(recall.getScore equals None)
  }

  it should "return correct values when data is zeros" in {
    val precision: Precision[Double] = new Precision[Double]()
    val recall: Recall[Double] = new Recall[Double]()
    precision.addObservation(IndexedSeq(), IndexedSeq(), null)
    recall.addObservation(IndexedSeq(), IndexedSeq(1,2,3), null)
    assert( math.abs(precision.getScore.get - 1.0 ) < 1e-5)
    assert( math.abs(recall.getScore.get - 0.0 ) < 1e-5)
  }

  it should "return correct score values" in {
    val precision: Precision[Double] = new Precision[Double]()
    val recall: Recall[Double] = new Recall[Double]()
    precision.addObservation(IndexedSeq(1,2,3,4,5,6,7), IndexedSeq(1,2,3,4,8), null)
    recall.addObservation(IndexedSeq(1,5,6,7), IndexedSeq(3,5,7), null)
    assert( math.abs(precision.getScore.get - 1.0 ) < 1e-5)
    assert( math.abs(recall.getScore.get - 0.0 ) < 1e-5)
  }


}
