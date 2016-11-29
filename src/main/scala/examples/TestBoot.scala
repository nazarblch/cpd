package examples

import java.util

import bootstrap._
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.MultivariateGaussian
import datasets.{Dataset, DenseVectorDataset, OneColumnDataset}
import models.standart.{NormalModel, NormalModelVecMean}
import patterns.StaticTrianglePattern
import statistics._
import statistics.likelihood_ratio._
import breeze.stats._

object TestBoot extends App {

  def test(h: Int, dim: Int): Double = {

    val model = new NormalModelVecMean(dim)

    // val r = new util.Random()
    val r = new MultivariateGaussian(DenseVector.zeros[Double](dim), DenseMatrix.eye[Double](dim))

    val dm = 0.3
    val n = 500

    def data_gen(dm: Double): DenseVectorDataset = {
      Dataset.applyVec(
        r.sample(h * 3).toIndexedSeq ++
          r.sample(h * 3).toIndexedSeq.map(x => x + DenseVector.fill(dim)(dm)))
    }

    val data = data_gen(dm)

    //  val wlrt = new SimpleWeightedLikelihoodRatioStatistic(model, h)
    val pattern = StaticTrianglePattern(h)
    //  val patt_wlrt = new PatternWeightedStatistic[Double, OneColumnDataset[Double]](pattern, wlrt)
    //  val max_patt_wlrt = new MaxWeightedStatistic[Double, OneColumnDataset[Double]](patt_wlrt)

    val lrt = new LikelihoodRatioStatistic(model, h)
    val patt_lrt = new PatternStatistic(pattern, lrt)
    val max_patt_lrt = new MaxStatistic(patt_lrt)

    val bootstrap = new EmpiricalBootstrap(max_patt_lrt)

    val SAMPLE_SIZE = 5000

    val sample = bootstrap.sample(data, SAMPLE_SIZE)
    val maxDist = new TailStatistic(sample.data)

    val q1 = maxDist.quantile(0.1)

    val ref_bootstrap = new RefDistBootstrap(data_gen, max_patt_lrt)

    val ref_sample = ref_bootstrap.sample(null, SAMPLE_SIZE)
    val ref_maxDist = new TailStatistic(ref_sample.data)

    val q2 = ref_maxDist.quantile(0.1)
    val p = ref_maxDist.getPValue(maxDist.quantile(0.1))
    val dp = math.abs(0.1 - p)

    println(h, dp, q2, q1, p)

    dp

  }

//  println(mean((1 to 30).map(i => test(40, 5)).toArray))
//  println(mean((1 to 30).map(i => test(40, 10)).toArray))
//  println(mean((1 to 30).map(i => test(40, 15)).toArray))
//  println(mean((1 to 30).map(i => test(40, 20)).toArray))
  println(mean((1 to 30).map(i => test(40, 3)).toArray))
  println(mean((1 to 30).map(i => test(40, 7)).toArray))



}
