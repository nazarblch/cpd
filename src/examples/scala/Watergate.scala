import breeze.linalg.DenseVector
import breeze.stats.distributions.{Exponential, Gaussian}
import cp_detectors.LRTOfflineDetector
import datasets.{OneColumnDataset, Column, DataHeader}
import models.standart.NormalModel
import patterns.TrianglePattern
import statistics.{PatternStatistic, MaxStatistic, PatternWeightedStatistic}
import statistics.likelihood_ratio.{ExtendedLikelihoodRatioStatistic, LikelihoodRatioStatistic, MeanVarWeightedLikelihoodRatioStatistic}
import viz.utils.PlotXY

/**
 * Created by nazar on 23/03/15.
 */
object Watergate extends App {

  def header: DataHeader = DataHeader(1)

  val col: Column[Double] = Column[Double](
    io.Source.fromFile("/Users/nazar/cpd/data/watergate-djia.dat").getLines().map(s => s.split(" ")(1).toDouble).toVector)
  val data = new OneColumnDataset[Double](header, col, true)

  val model = new NormalModel

  val lrt0 = new LikelihoodRatioStatistic(model, 30)
  val lrt1 = new LikelihoodRatioStatistic(model, 50)
  val lrt2 = new LikelihoodRatioStatistic(model, 70)
  val pl0 = new PlotXY("t", "LRT")
  //pl0.addline(data.getRowsIterator.map(_ - 0.1).toArray, "data(Y)")
  pl0.addline(lrt0.getValueWithLocations(data).map({case(k,v) => (k, v - 0.1)}), "LRT 30")
  pl0.addline(lrt1.getValueWithLocations(data), "LRT 50")
  pl0.addline(lrt2.getValueWithLocations(data).map({case(k,v) => (k, v + 0.1)}), "LRT 70")
  pl0.print("watergatedata.pdf")



  //val wstat = MeanVarWeightedLikelihoodRatioStatistic(model, 100)
  //val stat = PatternStatistic(model, 20)
  //val stat1 = PatternStatistic(model, 30)
  val conv0 = PatternStatistic(model, 30)
  val conv1 = PatternStatistic(model, 50)
  val conv2 = PatternStatistic(model, 70)
  //val lrt = new LikelihoodRatioStatistic(model, 100)

  val pl = new PlotXY("t", "conv")

  val detector = new LRTOfflineDetector[Double, OneColumnDataset[Double], DenseVector[Double]](model, 0.1, ExtendedLikelihoodRatioStatistic)
  detector.calibrate(data.subset(0, 300))
  val upper0 = detector.configurations(0).upperBound.get
  val bootLine0 = Array((0, upper0), (700, upper0))
  val upper1 = detector.configurations(1).upperBound.get
  val bootLine1 = Array((0, upper1), (700, upper1))
  val upper2 = detector.configurations(2).upperBound.get
  val bootLine2 = Array((0, upper2), (700, upper2))

  //pl.addline(stat.getValueWithLocations(data), "conv")
  //pl.addline(stat1.getValueWithLocations(data), "conv1")
  pl.addline(conv0.getValueWithLocations(data), "conv 30")
  pl.addline(conv1.getValueWithLocations(data), "conv 50")
  pl.addline(conv2.getValueWithLocations(data), "conv 70")
  pl.addline(bootLine0, "bound 30")
  pl.addline(bootLine1, "bound 50")
  pl.addline(bootLine2, "bound 70")
  //pl.addline(lrt.getValueWithLocations(data), "lrt")
  //pl.addline(wstat.getValue(data, Vector.fill(data.size)(r.draw())), "wlrt")

  //pl.print("watergate3.pdf")

}
