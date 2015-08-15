package bootstrap

import java.util.Random

import breeze.stats.distributions.{Exponential, Poisson, Gaussian}

object Params {
  val r = Poisson(1)
  def randOnesGeneratorRand: Double = {
    var res: Double = r.draw()
    if (res < 1e-8) res = 1e-8
    res
  }

//  val r = new Random()
//
//  def randOnesGeneratorRand: Double = {
//    var res = r.nextBoolean()
//    if(res) 2.0 else 0.00001
//  }
}
