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

  val col: Column[Double] = Column[Double](Gaussian(2,10).sample(1000))
  val data = new OneColumnDataset[Double](header, col, true)

  val col1: Column[Double] = Column[Double](Gaussian(4,10).sample(1000))
  val data1 = new OneColumnDataset[Double](header, col1, true)

  //val model = new AdaptiveGradientDescentOptimizer[Double](DenseVector(0,1), new NormalModel)
  val model = new NormalModel
  // val lrt = new ExtendedLikelihoodRatioStatistic[Double, OneColumnDataset[Double]](model, 50)

  val stat = MeanVarWeightedLikelihoodRatioStatistic(model, 100)

  val lrt_1 = new LikelihoodRatioStatistic[Double, OneColumnDataset[Double]](model, 100)

  val pl = new PlotXY("t", "conv")
  val r = Exponential(1)
  //val wdata = WeightedDataset(data ++ data1, )

  pl.addline(stat.getValue(data ++ data1, Vector.fill(2000)(r.draw())), "xi")
  pl.addline(lrt_1.getValue(data ++ data1), "lrt")

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


  pl.print("test.png")



}
