package utils


object Stat {
  def H(x: Double): Double = {
    if (x < 0.000001 || x > 0.99999) {
      0.0
    } else {
      x * math.log(x) + (1-x) * math.log(1-x)
    }
  }

}
