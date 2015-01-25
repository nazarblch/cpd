
import java.util.Random

import breeze.linalg.{DenseMatrix, DenseVector, inv}
import utils.sqrt



class ModelData(val Y: DenseVector[Double], val X: DenseMatrix[Double], val eps: DenseVector[Double]) {
  def length = Y.length
  def slice(s: Int, e: Int): ModelData = {
    new ModelData(Y.slice(s, e), X(s until e, ::), eps.slice(s, e))
  }
}

class Model(val data: ModelData, val h: Double, val theta1: DenseVector[Double], val theta2: DenseVector[Double], val sigma: Double = 1) {

  val window_size: Int = (data.length * h/2).toInt * 2
  assert(window_size > 20)
  val window_count: Int = data.length - window_size
  val n = data.length

  private def get_window_data(windowNumber: Int): (ModelData, ModelData) = {
    assert(windowNumber < window_count)
    (
      data.slice(windowNumber, windowNumber + window_size / 2),
      data.slice(windowNumber + window_size / 2, windowNumber + window_size))
  }

  private def get_window_D1D2(windowNumber: Int): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val w_data: (ModelData, ModelData) = get_window_data(windowNumber)
    val X1: DenseMatrix[Double] = w_data._1.X
    val X2: DenseMatrix[Double] = w_data._2.X
    val D1 = sqrt(X1.t * X1) / sigma
    val D2 = sqrt(X2.t * X2) / sigma
    (D1, D2)
  }

  private def get_mean_theta(s: Int, e: Int): DenseVector[Double] = {

    assert(s < e && e <= n)

    val w1: Double =
      if (s >= n/2) 0
      else (n/2 - s).toDouble / math.max(e - s, n/2 - s)

    theta1 * w1 + theta2 * (1 - w1)
  }

  private def get_window_theta(windowNumber: Int): (DenseVector[Double], DenseVector[Double]) = {
    (
      get_mean_theta(windowNumber, windowNumber + window_size/2),
      get_mean_theta(windowNumber + window_size/2, windowNumber + window_size)
      )
  }

  def opt_theta(s: Int, e: Int): DenseVector[Double] = {
    val sub_data = data.slice(s, e)
    val X = sub_data.X
    inv(X.t * X) * X.t * sub_data.Y
  }

  def L(s: Int, e: Int, theta: DenseVector[Double]): Double = {
    val sub_data = data.slice(s, e)
    val X = sub_data.X
    val Y = sub_data.Y
    - 0.5 / (sigma * sigma) * ((X * theta - Y) dot (X * theta - Y))
  }

  def L_opt(s: Int, e: Int): Double = L(s, e, opt_theta(s, e))

  def get_2dL(windowNumber: Int): Double = {
    val t1 = windowNumber
    val t2 = windowNumber + window_size/2
    val t3 = windowNumber + window_size
    2 * ( L_opt(t1, t2) + L_opt(t2, t3)
    - L_opt(t1, t3))
  }

  def get_window_xi(windowNumber: Int): (DenseVector[Double], DenseVector[Double]) = {
    val w_data: (ModelData, ModelData) = get_window_data(windowNumber)
    val X1: DenseMatrix[Double] = w_data._1.X
    val X2: DenseMatrix[Double] = w_data._2.X
    val eps1: DenseVector[Double] = w_data._1.eps
    val eps2: DenseVector[Double] = w_data._2.eps

    val gL1: DenseVector[Double] = X1.t * eps1 / (sigma * sigma)
    val gL2: DenseVector[Double] = X2.t * eps2 / (sigma * sigma)

    val D = get_window_D1D2(windowNumber)

    (inv(D._1) * gL1, inv(D._2) * gL2)
  }

  private def get_window_Sigma(windowNumber: Int): DenseMatrix[Double] = {
    val w_data: (ModelData, ModelData) = get_window_data(windowNumber)
    val S2: DenseMatrix[Double] = (w_data._1.X.t * w_data._1.X + w_data._2.X.t * w_data._2.X) / (4.0 * sigma * sigma)
    sqrt(S2)
  }


  def get_window_delta_xi(windowNumber: Int): DenseVector[Double] = {
    val xi = get_window_xi(windowNumber)
    val D = get_window_D1D2(windowNumber)
    val Sigma = get_window_Sigma(windowNumber)
    Sigma * (inv(D._2) * xi._2 - inv(D._1) * xi._1)
  }


  def get_delta_xi_sq(windowNumber: Int): Double = {
    val dxi = get_window_delta_xi(windowNumber)
    dxi dot dxi
  }


  private def get_rel_bias_by_number(windowNumber: Int): Double = {
    if (windowNumber + window_size < n/2 || windowNumber >= n/2) {
      0
    } else {
      1.0 - math.abs(windowNumber + window_size/2 - n/2).toDouble * 2 / window_size
    }
  }

  def get_delta_theta(windowNumber: Int): DenseVector[Double] = {
    val dtheta: DenseVector[Double] = (theta2 - theta1) * get_rel_bias_by_number(windowNumber)
    val Sigma = get_window_Sigma(windowNumber)
    Sigma * dtheta
  }

  def get_max_bias: Double = {
    val dtheta: DenseVector[Double] = theta2 - theta1
    val Sigma = get_window_Sigma(n/2 - window_size/2)
    (Sigma * dtheta).map(d => math.abs(d)).toArray.sum
  }

  def get_delta_xi_bias(windowNumber: Int): Double = {
    2 * (get_window_delta_xi(windowNumber) dot get_delta_theta(windowNumber))
  }

  def get_static_dL_part(windowNumber: Int): Double = {
    val Dtheta = get_delta_theta(windowNumber)
    Dtheta dot Dtheta
  }

  def get_long_dL_range: Array[Double] = {
    Range(n/2 - 3 * window_size, n/2 + 1 + 2 * window_size).map(i => get_static_dL_part(i)).toArray
  }

  def get_long_xi_bias_range: Array[Double] = {
    Range(n/2 - window_size, n/2 + 1).map(i => get_delta_xi_bias(i)).toArray
  }

  def get_long_delta_xi_range: Array[Double] = {
    Range(n/2 - window_size, n/2 + 1).map(i => get_delta_xi_sq(i)).toArray
  }

  def get_long_2dL_range: Array[Double] = {
    Range(n/2 - window_size, n/2 + 1).map(i => get_2dL(i)).toArray
  }

  def get_active_static_dL_range: Array[Double] = {
    Range(n/2 - window_size, n/2 + 1).map(i => get_static_dL_part(i)).toArray
  }

  def get_active_xi_bias_range: Array[Double] = {
    Range(n/2 - window_size, n/2 + 1).map(i => get_delta_xi_bias(i)).toArray
  }

  def get_active_delta_xi_range: Array[Double] = {
    Range(n/2 - window_size, n/2 + 1).map(i => get_delta_xi_sq(i)).toArray
  }

  def get_active_2dL_range: Array[Double] = {
    Range(n/2 - window_size, n/2 + 1).map(i => get_2dL(i)).toArray
  }

  def get_xi_sq_dot_trange: Double = {
    val xi_sq: DenseVector[Double] = DenseVector(get_active_delta_xi_range)
    val trange: DenseVector[Double] = DenseVector.range(-window_size / 2, window_size / 2 + 1).map(_.toDouble)
    xi_sq dot trange
  }

  def get_xi_bias_dot_trange: Double = {
    val xi_sq: DenseVector[Double] = DenseVector(get_active_xi_bias_range)
    val trange: DenseVector[Double] = DenseVector.range(-window_size / 2, window_size / 2 + 1).map(_.toDouble)
    xi_sq dot trange
  }

}


class ModelFactory(n: Int, theta1: DenseVector[Double], theta2: DenseVector[Double], h: Double) {

  def linage(s: Double, e: Double, n: Int, r: Random): DenseVector[Double] = {
    DenseVector.fill[Double](n)(s + r.nextDouble() * (e - s))
  }

  val X: DenseMatrix[Double] = DenseMatrix.zeros(n, theta1.length)

  val r = new Random(1)

  for (p <- 0 until theta1.length) {
    X(::, p) := linage(0.8, 1.2, n, r)
  }

  def apply(): Model = {

    assert(theta1.length == theta2.length)

    val r = new Random()

    val eps_1: Array[Double] = Array.fill[Double](n)(0).map(d => {
      val t1 = r.nextDouble()
      val t2 = r.nextDouble()
      math.sqrt(-2 * math.log(t1)) * math.cos(2 * math.Pi * t2)
    })
    val eps = DenseVector[Double](eps_1)

    val Y: DenseVector[Double] = X * theta1 + eps
    Y(n/2 until n) := X(n/2 until n, ::) * theta2 + eps(n/2 until n)

    val data = new ModelData(Y, X, eps)

    new Model(data, h, theta1, theta2)
  }
}
