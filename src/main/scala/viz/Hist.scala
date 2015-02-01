package viz

import breeze.linalg.DenseVector
import breeze.plot._

/**
 * Created by nazar on 1/28/15.
 */
class Hist(xlabel: String) {

  val f = Figure()
  val p = f.subplot(0)
  p.xlabel = xlabel
  p.legend = true


  var lc = 0

  def add(x: IndexedSeq[Double], name: String, cols: Int = 30) {
    val vec = DenseVector[Double](x.toArray)
    p += hist(vec, cols, name)
  }



  def print(path: String) {
    f.saveas(path)
  }

}


object HistApp extends App {
  val pl = new Hist("x")

  val g = breeze.stats.distributions.ChiSquared(3)

  pl.add(g.sample(100000), "name", 10000)

  pl.print("hist.png")
}