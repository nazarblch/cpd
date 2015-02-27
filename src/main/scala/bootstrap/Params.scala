package bootstrap

import java.util.Random

import breeze.stats.distributions.{Poisson, Gaussian}

object Params {
  val r = Poisson(1.0)
  def randOnesGeneratorRand: Double = {
    var res: Double = r.draw()
    if (res < 1e-8) res = 1e-8
    res
  }

//  val r = new Random()
//
//  def randOnesGeneratorRand: Double = {
//    var res: Double = r.nextGaussian() + 1
//    res
//  }
}
