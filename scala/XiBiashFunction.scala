import breeze.linalg.{DenseMatrix, DenseVector, inv}
import utils.PlotXY

import scala.collection.mutable.ArrayBuffer

object XiBiashFunction extends App {

  val n = 2000
  val theta1: DenseVector[Double] = DenseVector[Double](0.1, 0.5, 0.7)
  val delta = 0.1
  val theta2: DenseVector[Double] =  theta1 * (1- delta) + DenseVector(0.7, 0.4, 1.0) * delta
  val samples = 50

  val buf: ArrayBuffer[Double] = ArrayBuffer()

  println(new ModelFactory(n, theta1, theta2, 0.1).apply().get_max_bias )

  for (h <- 0.02 to 0.15 by 0.01) {
    val factory = new ModelFactory(n, theta1, theta2, h)
    val marr = Array.fill[Model](samples)(factory())
    buf += marr.map(model => {
      val d = model.get_xi_bias_dot_trange / model.get_max_bias
      d * d
    }).sum / samples
  }

  val pl = new PlotXY("log(nh)", "log(var / b^2)")

  val X = DenseVector((0.02 to 0.15 by 0.01).toArray).map(h => {
    math.log(n*h)
  })
  val XE: DenseMatrix[Double] = DenseMatrix.horzcat(DenseMatrix.ones[Double](X.length, 1), X.toDenseMatrix.t)
  val Y = DenseVector(buf.toArray).map(d => math.log(d))
  val coef: DenseMatrix[Double] =  inv(XE.t * XE) * XE.t * Y.toDenseMatrix.t

  println( coef.toString() )

  pl.addline(X, Y, "2 xi theta_12 dot trange var")

  pl.addline(X, (XE * coef.toDenseVector).toDenseVector, "2 xi theta_12 dot trange var approx")

  pl.print("/Users/nazar/change_point/img/xi_theta_dot_trange_var_1.pdf")

}
