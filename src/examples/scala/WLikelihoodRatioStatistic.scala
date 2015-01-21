import breeze.stats.distributions.Gaussian
import datasets.{Dataset, Column, DataHeader}
import models.standart.NormalModel
import patterns.{PatternMatcher, MatcherResult, TrianglePattern}
import statistics.likelihood_ratio.{WeightedLikelihoodRatioStatistic, LikelihoodRatioStatistic}
import viz.utils.PlotXY

import scala.collection.parallel.immutable.ParVector

object WLikelihoodRatioStatistic extends App {
  def header: DataHeader = new DataHeader(IndexedSeq("double"))

  val col: Column[Double] = new Column[Double](Gaussian(2,1).sample(300).toArray.par)
  val data = new Dataset[Double](header, ParVector(col), true)

  val col1: Column[Double] = new Column[Double](Gaussian(4,1).sample(300).toArray.par)
  val data1 = new Dataset[Double](header, ParVector(col1), true)

  val model = new NormalModel()
  val LRTs = new LikelihoodRatioStatistic[Double,Dataset[Double]](model, 50)

  val BootLRTs = new WeightedLikelihoodRatioStatistic[Double](model, 50)


  val pl = new PlotXY("t", "LRTs")

  pl.addline(LRTs.getValue(data ++ data1), "LRT")
  pl.addline(BootLRTs.getValue(data ++ data1, Gaussian(1,1).sample(600).toVector), "BootLRT")

  pl.print("test.png")
}
