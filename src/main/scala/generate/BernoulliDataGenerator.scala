package generate

import breeze.stats.distributions.Bernoulli

class BernoulliDataGenerator(val mean: Double = 0.5)
  extends DataGenerator[Boolean, Double] {

  var r = new Bernoulli(mean)

  override def next: Boolean = r.draw()

  override def update(param: Double): Unit = {
    r = new Bernoulli(param)
  }
}

