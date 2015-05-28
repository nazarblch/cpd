import breeze.stats.distributions.Gaussian
import cp_detectors.{CUSUMOfflineDetector, MeanVarOfflineDetector, OnlineAdapter}
import datasets.Dataset

/**
 * Created by nazar on 18/02/15.
 */
object MeanVarTest extends App {
//  val data: Array[IndexedSeq[Double]] = (Gaussian(2,1).sample(50) ++ Gaussian(50,1).sample(50)).map(x => Vector[Double](x)).toArray
//
//  val mvdet = new CUSUMOfflineDetector
//
//  println(mvdet.findAll(Dataset(data)).mkString(","))
//
//
//  val detector = new OnlineAdapter[Double](new CUSUMOfflineDetector , 20, 5)
//
//  var i = 0
//  for (d <- data) {
//    detector.addData(d)
//    println(i + ": " + detector.hasNewChangePoint)
//    i += 1
//  }
}
