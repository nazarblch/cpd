import bootstrap.{SmoothOnesGenerator, WeightedVectorBootstrap, Bootstrap}
import breeze.stats.distributions.Gaussian
import datasets.CellT._
import datasets.{DatasetConverter, DatasetLoader, Dataset}
import models.standart.NormalModel
import statistics.GaussianStatistic
import statistics.likelihood_ratio.{WeightedLikelihoodRatioStatistic, LikelihoodRatioStatistic}
import viz.Hist

/**
 * Created by nazar on 1/28/15.
 */
object Hist extends App {

  val data: Dataset[Double] = DatasetConverter.toNumeric(DatasetLoader.loadFromFile("data/test.csv"))

  LoadData.print(data)

  val model = new NormalModel
  val LRTs = new LikelihoodRatioStatistic[Double,Dataset[Double]](model, 10)
  val BootLRTs = new WeightedLikelihoodRatioStatistic[Double](model, 10)

  val bootstrap: Bootstrap[Double] = new WeightedVectorBootstrap[Double](new SmoothOnesGenerator, BootLRTs)

  val pl = new Hist("x")

  val g = breeze.stats.distributions.ChiSquared(1).sample(100000).map(x => math.sqrt(x))

  pl.add(g, "chi", 1000)

  pl.add(bootstrap.sample(data, 100000).toArray, "boot", 1000)



  pl.print("hist.png")
}
