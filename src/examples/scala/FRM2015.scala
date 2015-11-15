import breeze.linalg.DenseVector
import datasets.{DatasetLoader, DatasetConverter}
import models.standart.RegressionModel
import patterns.TrianglePattern
import statistics.PatternStatistic
import statistics.likelihood_ratio.LikelihoodRatioStatistic
import viz.utils.PlotXY


object FRM2015 {

  val data1 = DatasetConverter.toNumeric(DatasetLoader.loadFromFileMulticol("/Users/buzun/cpd/data/frm2015.csv", 1, 20, ";"))
  val data2 = DatasetConverter.toNumeric(DatasetLoader.loadFromFileMulticol("/Users/buzun/cpd/data/frm2015.csv", 201, 207, ";"))

  val data =  data1.concat(data2)
  val h = 90

  def calcLRT(num: Int): DenseVector[Double] = {
    val modelL1 = new RegressionModel(data.dim - 1, num, data.header)
    println(num)
    val pattern = new TrianglePattern(2 * h)
    val lrt = new LikelihoodRatioStatistic(modelL1, h)
    val patt_lrt = new PatternStatistic(pattern, lrt)
    DenseVector(patt_lrt.getValue(data))
  }

  val avgLRT = Range(0, 5).map(i => {
    calcLRT(i)
  }).reduce(_+_).map(_ / 5)

  val pl = new PlotXY("t", "LRT")
  pl.addline(avgLRT, "")
  pl.p.legend = false
  pl.print("jumps.pdf")

  println(avgLRT.toArray.mkString(","))

}
