package generate.patterns

import java.util

import breeze.stats.distributions.Gaussian
import datasets.{Dataset, OneColumnDataset}


object PatternGenerator {

  val r = new util.Random()
  val sigma = 1.0

  def fromToLine(from: Double, to: Double, duration: Int): Array[Double] = {
    val x0 = from
    val dx = (to - from) / duration
    Array.range(0, duration).map(i => x0 + i * dx)
  }

  def genMeanJumpData(n: Int, strength: Double): OneColumnDataset[Double] = {
    Dataset(
      Array.fill(n / 2)(sigma * r.nextGaussian()) ++
        Array.fill(n / 2)(strength + sigma * r.nextGaussian()))
  }

  def genMeanOutlierData(n: Int, strength: Double, duration: Int): OneColumnDataset[Double] = {
    Dataset(
      Array.fill(n / 2)(sigma * r.nextGaussian()) ++
        Array.fill(duration)(strength + sigma * r.nextGaussian()) ++
         Array.fill(n / 2)(sigma * r.nextGaussian()))
  }

  def genSigmaJumpData(n: Int, strength: Double): OneColumnDataset[Double] = {
    Dataset(
      Array.fill(n / 2)(sigma * r.nextGaussian()) ++
        Array.fill(n / 2)((strength + sigma) * r.nextGaussian()))
  }

  def genMeanTransData(n: Int, strength: Double, duration: Int): OneColumnDataset[Double] = {
    Dataset(
      Array.fill(n / 2)(sigma * r.nextGaussian()) ++
        fromToLine(0, strength, duration).map(mi => mi + sigma * r.nextGaussian()) ++
          Array.fill(n / 2)(strength + sigma * r.nextGaussian()))
  }

  def genSigmaTransData(n: Int, strength: Double, duration: Int): OneColumnDataset[Double] = {
    Dataset(
      Array.fill(n / 2)(sigma * r.nextGaussian()) ++
        fromToLine(0, strength, duration).map(mi => (mi + sigma) * r.nextGaussian()) ++
          Array.fill(n / 2)((strength + sigma) * r.nextGaussian()))
  }

}
