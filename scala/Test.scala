import breeze.linalg.DenseVector
import utils.PlotXY

object Run extends App {

  val n = 10000
  val theta1: DenseVector[Double] = DenseVector(0.1, 0.5)
  val delta = 0.15
  val theta2: DenseVector[Double] =  theta1 * (1- delta) + DenseVector(0.7, 0.4) * delta
  val h = 0.1

  val model: Model = new ModelFactory(n, theta1, theta2, h).apply()

  println(math.sqrt((theta2 - theta1) dot (theta2 - theta1)))

  val pl = new PlotXY("t", "L")

  pl.addline(
    DenseVector(
      model.get_active_static_dL_range
    ),
    "theta_12^2")


  pl.addline(
    DenseVector(
      model.get_active_delta_xi_range
    ),
    "xi^2")

  pl.addline(
    DenseVector(
      model.get_active_xi_bias_range
    ),
    "2 theta_12 xi")


  pl.addline(
    DenseVector(
      model.get_active_2dL_range.map(_ - theta1.length)
    ),
    "2dL - p")

  pl.print("/Users/nazar/change_point/img/sep_plot_025_long.pdf")


}
