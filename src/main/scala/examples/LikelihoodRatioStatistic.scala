package examples

import datasets._
import generate.patterns.PatternGenerator
import models.ParametricIIDModel
import models.standart.NormalModel
import patterns._
import statistics.likelihood_ratio.LikelihoodRatioStatistic
import viz.utils.PlotXY

object LikelihoodRatioStatistic extends App {


  def getstats(h: Int, model: ParametricIIDModel[Double, OneColumnDataset[Double]]) = {
    val pattern = new TrianglePattern(2 * h)
    val lrt = new LikelihoodRatioStatistic(model, h)
    // val patt_lrt = new PatternStatistic[Double, OneColumnDataset[Double]](pattern, lrt)
    //(lrt, patt_lrt)
  }

  val prefix = "m2_trans"

  PatternGenerator.sigma = 1
  val data = PatternGenerator.genSigmaTransData(1000, 5, 500)
  //val lrt1 = new LikelihoodRatioStatistic(new NormalModelMean, 100)
  val lrt2 = new LikelihoodRatioStatistic(new NormalModel, 200)



  val pl = new PlotXY("time", "LRT statistics")
  //pl.addline(lrt_20.getValueWithLocations(data), "h = 20")
  //pl.addline(lrt1.getValueWithLocations(data), "h = 100")
  pl.addline(lrt2.getValueWithLocations(data), "h = 100")
  pl.addline(data.data.data.zipWithIndex.map(_.swap).toArray, "data")
  //pl.addline(lrt_100.getValueWithRightLocations(data), "h = 100")
  pl.print(prefix + "sigma_transit.pdf")

  //val pl1 = new PlotXY("t", "pattern conv statistics")
  //pl1.addline(pat_lrt_20.getValueWithLocations(data), "h = 20")
  //pl1.addline(pat_lrt_50.getValueWithLocations(data), "h = 50")
  //val res = pat_lrt_100.getValueWithRightLocations(data)
  //pl1.addline(pat_lrt_100.getValueWithRightLocations(data), "h = 100")
  //pl1.print(prefix + "mlscale_conv.pdf")




}
