package generate

import breeze.stats.distributions.Poisson


class PoissonDataGenerator(val mean: Double = 1)
  extends DataGenerator[Int, Double] {

  var r = Poisson(mean)

  override def next: Int = r.draw()

  override def update(param: Double): Unit = {
    println("m=" + param)
    r = Poisson(param)
  }
}
