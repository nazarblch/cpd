import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import cp_detectors.{LRTOnlineDetector, D1BayesianOnlineCPDetector}
import datasets.Dataset
import models.standart.{NormalModelMean, NormalModel}
import org.ddahl.jvmr.RInScala
import patterns.{StaticHalfTrianglePattern, HalfTrianglePattern, TrianglePattern}
import statistics.{PatternStatistic, PatternWeightedStatistic}
import statistics.likelihood_ratio.LikelihoodRatioStatistic
import viz.utils.PlotXY

/**
 * Created by nazar on 17/02/15.
 */
object LRTOnlineTest extends App {

  val model = new NormalModelMean
  val detector = new LRTOnlineDetector(model)

  def genPattLRTWithBound(size: Int, v: Double) = {

    val trainData = Gaussian(-5,v).sample(size).map(x => DenseVector(x))

    detector.init(Dataset.applyVec(trainData))
    val upper: Double = detector.upperBounds(2)

    val lrt = new LikelihoodRatioStatistic[Double,Dataset[Double]](model, detector.WINDOW_SIZES(2))

    val pattern = new StaticHalfTrianglePattern(detector.WINDOW_SIZES(2))
    val patt_lrt = new PatternStatistic(pattern, lrt)

    val res = patt_lrt.getValueWithLocations(trainData)

    val bound = res.map(p => upper)

    (res.map(_._2), bound, trainData.map(_(0)).toArray.take(res.length))
  }

  def genPattLRTWithBound(size: Int, v: Double, dm: Double) = {

    assert(size % 2 == 0)

    val trainData1 = Gaussian(-5 + dm, v).sample(size/2).map(x => DenseVector(x))
    val trainData2 = Gaussian(-5 - dm, v).sample(size/2).map(x => DenseVector(x))
    val trainData = trainData1.zip(trainData2).flatMap(p => Seq(p._1, p._2))

    detector.init(Dataset.applyVec(trainData))
    val upper: Double = detector.upperBounds(2)

    val lrt = new LikelihoodRatioStatistic[Double,Dataset[Double]](model, detector.WINDOW_SIZES(2))

    val pattern = new StaticHalfTrianglePattern(detector.WINDOW_SIZES(2))
    val patt_lrt = new PatternStatistic(pattern, lrt)

    val res = patt_lrt.getValueWithLocations(trainData)

    val bound = res.map(p => upper)

    (res.map(_._2), bound, trainData.map(_(0)).toArray.take(res.length))
  }

  val pl = new PlotXY("t", "LRT conv")

  val res2 = genPattLRTWithBound(200, 1, 3)
  val res1 = genPattLRTWithBound(200, 1)

  pl.addline(res1._1 ++ res2._1, "conv")
  pl.addline(res1._2 ++ res2._2, "bound")
  pl.addline((res1._3 ++ res2._3), "data")

  pl.print("pa_change.pdf")



  //    var i = 0
//    for (d <- data) {
//      detector.addData(d)
//      println(i + ": " + detector.hasNewChangePoint)
//      i += 1
//    }




}