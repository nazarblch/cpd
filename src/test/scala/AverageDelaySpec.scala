import org.scalatest.FlatSpec
import quality.AverageDelay

class AverageDelaySpec extends FlatSpec {

  "A delay " should "return None when no experiments are provided" in {
    val delay: AverageDelay[Double] = new AverageDelay[Double]()
    assert(delay.getScore equals None)
  }

  it should "return correct None when data is zeros" in {
    val delay: AverageDelay[Double] = new AverageDelay[Double]()
    delay.addObservation(IndexedSeq(), IndexedSeq(), null)
    delay.addObservation(IndexedSeq(), IndexedSeq(1,2,3), null)
    assert(delay.getScore equals None)
  }

  it should "return correct score values" in {
    val delay: AverageDelay[Double] = new AverageDelay[Double]()
    delay.addObservation(IndexedSeq(1,2,3,4,5,6,7), IndexedSeq(1,2,3,4,8), null)
    delay.addObservation(IndexedSeq(1,5,6,7), IndexedSeq(3,5,7), null)
    assert( math.abs(delay.getScore.get - 7.0/6 ) < 1e-5)
  }

}
