package bootstrap

import java.util.Random
import breeze.stats._
import breeze.stats.distributions.Gaussian

class SmoothOnesGenerator () {
  def generateVector(size: Int): Vector[Double] = {
    Vector.fill[Double](size)(Params.randOnesGeneratorRand)
  }
}
