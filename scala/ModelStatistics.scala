import breeze.linalg.DenseVector

object ModelStatistics extends App {

  val n = 5000
  val theta1: DenseVector[Double] = DenseVector(0.1, 0.5, 0.7)
  val delta = 0.1
  val theta2: DenseVector[Double] =  theta1 * (1- delta) + DenseVector(0.7, 0.4, 1.0) * delta
  val h = 0.1
  val samples = 10000

  val factory = new ModelFactory(n, theta1, theta2, h)
  val model = factory()
  val marr = Array.fill[Model](samples)(factory())

  println("E|xi|^2")
  println(marr.map(_.get_delta_xi_sq(10)).sum / samples)
  println("Var|xi|^2")
  println(marr.map(xi => (xi.get_delta_xi_sq(10) - theta1.length)*(xi.get_delta_xi_sq(10) - theta1.length)).sum / samples)

  println("delta theta_12")
  println(model.get_delta_theta(2250).toString())


  println("E xi")
  println(marr.map(_.get_window_delta_xi(10) / samples.toDouble).reduce(_+_).toString)
  println("Var (2 xi * theta_12)")
  println(marr.map(xi => xi.get_delta_xi_bias(2250) * xi.get_delta_xi_bias(2250)).sum / samples)
  println("expected Var (2 xi * theta_12)")
  println(4 * (model.get_delta_theta(2250) dot model.get_delta_theta(2250)))

}
