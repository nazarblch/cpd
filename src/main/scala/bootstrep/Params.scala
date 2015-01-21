package bootstrep

import java.util.Random

import breeze.stats.distributions.Gaussian

object Params {
  val r = new Random(1)
  def randOnesGeneratorRand: Double = r.nextGaussian() + 1
}
