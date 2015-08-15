import java.io.FileWriter
import java.util

import bootstrap.{RefDistBootstrap, SmoothOnesGenerator, WeightedBootstrap, Bootstrap}

import breeze.stats.distributions.Gaussian
import datasets.{Dataset, OneColumnDataset}
import models.standart.{NormalModelMean, NormalModel}
import patterns.{StaticTrianglePattern, TrianglePattern}
import statistics.likelihood_ratio._
import statistics._

import scala.util.Random

object TestBoot extends App {

  val model = new NormalModelMean(1)
  val h = 50

  val r = new util.Random()

  val m: Double = 2
  val dm = 0.1

  def data_gen(dm: Double = 0.0): OneColumnDataset[Double] = {
    Dataset(
      Array.fill(2*h + 30)(m + r.nextGaussian()) ++
        Array.fill(2*h + 30)(m + dm + r.nextGaussian()))
  }

  val data = data_gen(dm)

  val wlrt = new SimpleWeightedLikelihoodRatioStatistic(model, h)
  val pattern = new TrianglePattern(2 * h)
  val patt_wlrt = new PatternWeightedStatistic[Double, OneColumnDataset[Double]](pattern, wlrt)
  val max_patt_wlrt = new MaxWeightedStatistic[Double, OneColumnDataset[Double]](patt_wlrt)

  val bootstrap: Bootstrap[Double, OneColumnDataset[Double]] =
       new WeightedBootstrap[Double, OneColumnDataset[Double]](new SmoothOnesGenerator, max_patt_wlrt)

  val SAMPLE_SIZE = 1000

  val sample = bootstrap.sample(data, SAMPLE_SIZE)
  val maxDist = new TailStatistic(sample.data)

  val q1 = maxDist.quantile(0.1)

  val lrt = new LikelihoodRatioStatistic(model, h)
  val patt_lrt = new PatternStatistic[Double, OneColumnDataset[Double]](pattern, lrt)
  val max_patt_lrt = new MaxStatistic[Double, OneColumnDataset[Double]](patt_lrt)

  val ref_bootstrap: RefDistBootstrap[Double, OneColumnDataset[Double]] =
    new RefDistBootstrap[Double, OneColumnDataset[Double]](data_gen, max_patt_lrt)

  val ref_sample = ref_bootstrap.sample(SAMPLE_SIZE)
  val ref_maxDist = new TailStatistic(ref_sample.data)

  val q2 = ref_maxDist.quantile(0.1)
  val p = ref_maxDist.getPValue(maxDist.quantile(0.1))
  val dp = math.abs(0.1 - p)

  println(h,dp,q2,q1,p)

  val fw = new FileWriter("/home/nazar/cpd/data/p_values_cp.txt", true)

  fw.write(dm + "," + dp + ",TbS\n")
  fw.close()
}
