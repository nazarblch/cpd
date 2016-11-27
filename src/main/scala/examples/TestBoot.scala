package examples

import java.util

import bootstrap._
import datasets.{Dataset, OneColumnDataset}
import models.standart.NormalModel
import patterns.StaticTrianglePattern
import statistics._
import statistics.likelihood_ratio._

object TestBoot extends App {

  val model = new NormalModel
  val h = 200

  val r = new util.Random()

  val m: Double = 2
  val dm = 0.0
  val n = 200

  def data_gen(dm: Double = dm): OneColumnDataset[Double] = {
    Dataset(
      Array.fill(4 * h)(m + r.nextGaussian()) ++
        Array.fill(4 * h)(m + dm + r.nextGaussian()))
  }

  val data = data_gen(dm)

//  val wlrt = new SimpleWeightedLikelihoodRatioStatistic(model, h)
    val pattern = new StaticTrianglePattern(2 * h)
//  val patt_wlrt = new PatternWeightedStatistic[Double, OneColumnDataset[Double]](pattern, wlrt)
//  val max_patt_wlrt = new MaxWeightedStatistic[Double, OneColumnDataset[Double]](patt_wlrt)

  val lrt = new LikelihoodRatioStatistic(model, h)
  val patt_lrt = new PatternStatistic[Double, OneColumnDataset[Double]](pattern, lrt)
  val max_patt_lrt = new MaxStatistic[Double, OneColumnDataset[Double]](lrt)

  val bootstrap: Bootstrap[Double, OneColumnDataset[Double]] =
       new EmpiricalBootstrap[Double, OneColumnDataset[Double]](max_patt_lrt)

  val SAMPLE_SIZE = 1000

  val sample = bootstrap.sample(data, SAMPLE_SIZE)
  val maxDist = new TailStatistic(sample.data)

  val q1 = maxDist.quantile(0.1)

  val ref_bootstrap: RefDistBootstrap[Double, OneColumnDataset[Double]] =
    new RefDistBootstrap[Double, OneColumnDataset[Double]](data_gen, max_patt_lrt)

  val ref_sample = ref_bootstrap.sample(SAMPLE_SIZE)
  val ref_maxDist = new TailStatistic(ref_sample.data)

  val q2 = ref_maxDist.quantile(0.1)
  val p = ref_maxDist.getPValue(maxDist.quantile(0.1))
  val dp = math.abs(0.1 - p)

  println(h,dp,q2,q1,p)


}
