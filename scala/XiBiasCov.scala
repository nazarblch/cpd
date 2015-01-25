import breeze.linalg.DenseVector
import utils.PlotXY

object XiBiasCov extends App {

  val n = 2000
  val theta1: DenseVector[Double] = DenseVector(0.1, 0.5, 0.7)
  val delta = 0.1
  val theta2: DenseVector[Double] =  theta1 * (1- delta) + DenseVector(0.7, 0.4, 1.0) * delta
  val h = 0.1
  val samples = 1000
  val nh = (n*h).toInt

  val factory = new ModelFactory(n, theta1, theta2, h)
  val model = factory()
  val marr = Array.fill[Model](samples)(factory())

  val pl = new PlotXY("t", "2 xi theta_12 cov")

  for (t <- Range(n/2 - nh, n/2, 20)) {

    val xi_bias_cov: DenseVector[Double] = marr.map(m => {
      val cxi = m.get_delta_xi_bias(t)
      DenseVector.range(n/2 - nh/2 - 110, n/2 - nh/2 + 110).map(j => {
        val cxi_2 = m.get_delta_xi_bias(j)
        cxi * cxi_2 / samples.toDouble
      })
    }).reduce(_+_)

    pl.addline(
      xi_bias_cov,
      "2 xi theta_12 cov t = " + t.toString)

  }

  pl.print("/Users/nazar/change_point/img/xi_bias_cov.pdf")


}