import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import cp_detectors.{LRTOnlineDetector}
import datasets.{OneColumnDataset, Dataset}
import models.standart.{NormalModelMean, NormalModel}

import patterns.{StaticHalfTrianglePattern, HalfTrianglePattern, TrianglePattern}
import statistics.{PatternStatistic, PatternWeightedStatistic}
import statistics.likelihood_ratio.{MeanVarWeightedLikelihoodRatioStatistic, LikelihoodRatioStatistic}
import viz.utils.PlotXY

/**
 * Created by nazar on 17/02/15.
 */
object LRTOnlineTest extends App {

  val model = new NormalModel
  val detector = new LRTOnlineDetector(model, MeanVarWeightedLikelihoodRatioStatistic)

  def genPattLRTWithBound(size: Int, v: Double) = {

    val trainData = Gaussian(-5,v).sample(size)

    detector.init(Dataset.apply(trainData))
    val upper: Double = detector.configurations(2).upperBound.get

    val lrt = new LikelihoodRatioStatistic[Double, OneColumnDataset[Double]](model, detector.WINDOW_SIZES(2))

    val pattern = new StaticHalfTrianglePattern(detector.WINDOW_SIZES(2))
    val patt_lrt = new PatternStatistic(pattern, lrt)

    val res = patt_lrt.getValueWithLocations(Dataset.apply(trainData))

    val bound = res.map(p => upper)

    (res.map(_._2), bound, trainData.toArray.take(res.length))
  }

  def genPattLRTWithBound(size: Int, v: Double, dm: Double) = {

    assert(size % 2 == 0)

    val trainData1 = Gaussian(-5 + dm, v).sample(size/2)
    val trainData2 = Gaussian(-5 - dm, v).sample(size/2)
    val trainData = trainData1.zip(trainData2).flatMap(p => Seq(p._1, p._2))

    detector.init(Dataset.apply(trainData))
    val upper: Double = detector.configurations(2).upperBound.get

    val lrt = new LikelihoodRatioStatistic[Double, OneColumnDataset[Double]](model, detector.WINDOW_SIZES(2))

    val pattern = new StaticHalfTrianglePattern(detector.WINDOW_SIZES(2))
    val patt_lrt = new PatternStatistic(pattern, lrt)

    val res = patt_lrt.getValueWithLocations(Dataset.apply(trainData))

    val bound = res.map(p => upper)

    (res.map(_._2), bound, trainData.toArray.take(res.length))
  }



//  val res2 = genPattLRTWithBound(200, 1, 3)
//  val res1 = genPattLRTWithBound(200, 1)

//  pl.addline(res1._1 ++ res2._1, "conv")
//  pl.addline(res1._2 ++ res2._2, "bound")
//  pl.addline((res1._3 ++ res2._3), "data")


    val data = Gaussian(0, 1).sample(200)
    val data1 = Gaussian(0.2, 1.3).sample(200)

    detector.init(Dataset.apply(data))
    println("h = " + detector.configurations.map(_.upperBound).mkString(","))

    var i = 0
    for (d <- data ++ data1) {
      detector.addData(d)
      if (detector.hasNewChangePoint) println(i + ": cp")
      i += 1
    }

  val pl = new PlotXY("t", "LRT conv")

  detector.convs.foreach(p => {
    pl.addline(p._2.toArray, p._1.toString)
  })

  pl.print("pa_change.pdf")


  val pl1 = new PlotXY("t", "LRT lrt")

  detector.LRT.foreach(p => {
    pl1.addline(p._2.toArray, p._1.toString)
  })

  pl1.print("pa_change_1.pdf")




}