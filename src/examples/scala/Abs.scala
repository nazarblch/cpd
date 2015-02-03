/**
 * Created by nazar on 13/01/15.
 */
abstract class Abs(val e: Int) {
  def hren(a: Int): Int = a + e
}


trait Abs1 {

  val e: Int

  def hren(a: Int): Int = a + e
}


object AbsTest extends App {

  val a = new Abs1 {
    override val e: Int = 1
  }

  val b = new Abs1 {
    override val e: Int = 3
  }


  println(a.hren(1))


}