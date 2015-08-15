import java.util.Random

import bootstrap.{WeightedVectorBootstrap, SmoothOnesGenerator, WeightedBootstrap, Bootstrap}
import breeze.linalg.DenseVector
import breeze.stats.distributions.{Exponential, Poisson, Gaussian}
import cp_detectors.LRTOfflineDetector
import datasets._
import likelihood_optimize.AdaptiveGradientDescentOptimizer
import models.standart.{NormalModelMean, NormalModel}
import patterns.{StaticTrianglePattern, PatternMatcher, MatcherResult, TrianglePattern}
import statistics.{MaxStatistic, PatternWeightedStatistic, PatternStatistic}
import statistics.likelihood_ratio.{ExtendedLikelihoodRatioStatistic, MeanVarWeightedLikelihoodRatioStatistic, WeightedLikelihoodRatioStatistic, LikelihoodRatioStatistic}
import viz.utils.PlotXY

import scala.collection.parallel.immutable.ParVector

object LikelihoodRatioStatistic extends App {

  def header: DataHeader = DataHeader(1)

  val load = io.Source.fromFile("/home/nazar/Downloads/MyData.csv").getLines().drop(1)
    .map(_.trim.toDouble).toVector.slice(0, 600000)

  val col: Column[Double] = Column[Double](load)
  val data = new OneColumnDataset[Double](header, col, true)


  val model = new NormalModelMean(1)
  // val lrt = new ExtendedLikelihoodRatioStatistic[Double, OneColumnDataset[Double]](model, 50)

  //val stat = MeanVarWeightedLikelihoodRatioStatistic(model, 100)

  val h = 50
  val pattern = new TrianglePattern(2 * h)
  val lrt = new LikelihoodRatioStatistic(model, h)
  val patt_lrt = new PatternStatistic[Double, OneColumnDataset[Double]](pattern, lrt)

  val pl = new PlotXY("t", "conv")
  pl.addline(patt_lrt.getValue(data), "conv")
  //pl.addline(load.map(_ * 300).toArray, "data")

  //pl.addline(lrt.get_delta_xi_norm(wdata), "xi")
  //pl.addline(lrt_1.getValue(wdata.toDataset), "stat")

//  val pattern = new TrianglePattern(40)
//  val patt_lrt = new PatternStatistic[Double, Dataset[Double]](pattern, lrt)
//
//  val parr_lrt_res = patt_lrt.getValueWithLocations(data ++ data1)
//
//  val wlrt = new MeanVarWeightedLikelihoodRatioStatistic[Double](model, 20)
//
//  val patt_wlrt = new PatternWeightedStatistic[Double](pattern, wlrt)
//  //val max_patt_wlrt = new MaxStatistic[Double](patt_wlrt)
//
//  val bootstrap: Bootstrap[Double] = new WeightedVectorBootstrap[Double](new SmoothOnesGenerator, patt_wlrt)
//
//
//  val sample: DenseVector[Double] = bootstrap.sample(data ++ data1, 200)



//  val pl = new PlotXY("t", "conv")
//
//  pl.addline(parr_lrt_res, "stat")
//
//  pl.addline(sample, "sample")


  pl.print("res50.png")




}
