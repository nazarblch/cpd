import java.util.Random

import bootstrap.{WeightedVectorBootstrap, SmoothOnesGenerator, WeightedBootstrap, Bootstrap}
import breeze.linalg.DenseVector
import breeze.stats.distributions.{Exponential, Poisson, Gaussian}
import cp_detectors.LRTOfflineDetector
import datasets._
import generate.patterns.PatternGenerator
import likelihood_optimize.AdaptiveGradientDescentOptimizer
import models.standart.{NormalModelMean, NormalModel}
import patterns._
import statistics.{MaxStatistic, PatternWeightedStatistic, PatternStatistic}
import statistics.likelihood_ratio.{ExtendedLikelihoodRatioStatistic, MeanVarWeightedLikelihoodRatioStatistic, WeightedLikelihoodRatioStatistic, LikelihoodRatioStatistic}
import viz.utils.PlotXY

import scala.collection.parallel.immutable.ParVector

object LikelihoodRatioStatistic extends App {



  val model = new NormalModel


  def getstats(h: Int) = {
    val pattern = new TrianglePattern(2 * h)
    val lrt = new LikelihoodRatioStatistic(model, h)
    val patt_lrt = new PatternStatistic[Double, OneColumnDataset[Double]](pattern, lrt)
    (lrt, patt_lrt)
  }

  val prefix = "m2_trans"

  val data = PatternGenerator.genMeanTransData(400, 150, 200)
  val (lrt_20, pat_lrt_20) = getstats(20)
  val (lrt_50, pat_lrt_50) = getstats(50)
  val (lrt_100, pat_lrt_100) = getstats(100)



  val pl = new PlotXY("t", "LRT statistics")
  pl.addline(lrt_20.getValueWithLocations(data), "h = 20")
  pl.addline(lrt_50.getValueWithLocations(data), "h = 50")
  //pl.addline(lrt_100.getValueWithRightLocations(data), "h = 100")
  pl.print(prefix + "mlscale_lrt.pdf")

  val pl1 = new PlotXY("t", "pattern conv statistics")
  pl1.addline(pat_lrt_20.getValueWithLocations(data), "h = 20")
  pl1.addline(pat_lrt_50.getValueWithLocations(data), "h = 50")
  //val res = pat_lrt_100.getValueWithRightLocations(data)
  //pl1.addline(pat_lrt_100.getValueWithRightLocations(data), "h = 100")
  pl1.print(prefix + "mlscale_conv.pdf")




}
