package examples

import bootstrap._
import breeze.linalg.DenseVector
import datasets.{DatasetLoader, DenseVectorDataset, MultiColumnDataset}
import models.standart.NormalModelVec
import patterns.TrianglePattern
import statistics._
import statistics.likelihood_ratio.LikelihoodRatioStatistic
import viz.utils.PlotXY

/**
  * Created by buzun on 07/11/16.
  */
object MultiScale extends App {

  val data = DatasetLoader.loadFromFileMulticolDouble("/Users/buzun/Downloads/forNazarrrr.csv")
  println(data.getRow(0).toArray.mkString(","))

  val h = 100

  val model = new NormalModelVec(data.getColumns.size)

  val lrt = new LikelihoodRatioStatistic(model, h)
  val pattern = new TrianglePattern(2 * h)
  val patt_lrt = new PatternStatistic(pattern, lrt)
  val max_patt_lrt = new MaxStatistic(patt_lrt)
  // val max_wlrt = new MaxStatistic[Vector[Double], MultiColumnDataset[Double]](lrt)

  val bootstrap = new EmpiricalBootstrap[DenseVector[Double], DenseVectorDataset](max_patt_lrt)

//  val pvalue = 0.05
//  val bootstrapIterations = 100
//  val bound = bootstrap.quantile(1 - pvalue, data, bootstrapIterations)

//  println(bound)

  val pl = new PlotXY("time", "LRT statistics")
  pl.addline(patt_lrt.getValueWithLocations(data), "h = 100")
//  pl.addline(Array.fill(1000)(bound), "bound")


}
