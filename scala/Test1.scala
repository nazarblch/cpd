import breeze.linalg.DenseMatrix
import breeze.stats.distributions.Gaussian
import utils.sqrt

object Test1 extends App {
  val g = Gaussian(0,1)
  val data = Array.fill[Double](10000)(Gaussian(0,1).draw()).map(i => i*i)
  println(data.sum / 10000)
  println(data.map(d => (d-1)*(d-1)).sum/10000)

  val d = DenseMatrix((2.0, 1.0), (1.0, 4.0))
  println(sqrt(d * d).toString())

}
