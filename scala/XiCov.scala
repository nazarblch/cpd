import breeze.linalg.DenseVector
import utils.PlotXY

object XiCov extends App {

  val n = 2000
  val theta1: DenseVector[Double] = DenseVector(0.1, 0.5, 0.7)
  val delta = 0.1
  val theta2: DenseVector[Double] =  theta1 * (1- delta) + DenseVector(0.7, 0.4, 1.0) * delta
  val h = 0.05
  val samples = 500

  val factory = new ModelFactory(n, theta1, theta2, h)
  val model = factory()
  val marr = Array.fill[Model](samples)(factory())

  val xi_sq_cov: DenseVector[Double] = marr.map(m => {
    val cxi = m.get_delta_xi_sq(n/2) - theta1.length
    DenseVector.range(n/2 - 200, n/2 + 200).map(j => {
      val cxi_2 = m.get_delta_xi_sq(j) - theta1.length
      cxi * cxi_2 / samples.toDouble
    })
  }).reduce(_+_)

  val xi_1_cov: DenseVector[Double] = marr.map(m => {
    val cxi = m.get_window_delta_xi(n/2)(0)
    DenseVector.range(n/2 - 200, n/2 + 200).map(j => {
      val cxi_2 = m.get_window_delta_xi(j)(0)
      cxi * cxi_2 / samples.toDouble
    })
  }).reduce(_+_)

  val pl = new PlotXY("t", "cov")

  pl.addline(
      xi_sq_cov,
    "xi^2 cov")

  pl.addline(
    xi_1_cov,
    "xi_1 cov")

  pl.print("/Users/nazar/change_point/img/xi_sq_cov_1.pdf")


}
