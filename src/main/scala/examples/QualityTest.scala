package examples

import breeze.stats._
import cp_detectors.LRTOfflineDetector
import models.standart.NormalModelVecMean
import patterns._

class PowerAndSD(private val sd : Double,
                 private val power : Double,
                 private val sdsd : Double) {
  override def toString: String = {
    "POWER " + power + "\n" +  "SD    " + sd + " +- " + 1.96 * sdsd
  }
}

object PowerAndSD {
  def apply(trueCPLocation : Int)(res : Seq[Option[Int]]) = {
    val power = res.count(_.isDefined).toDouble / res.length
    val detectedPositions = res.flatten.map(x => math.abs(x - trueCPLocation).toDouble)
    val sd = mean(detectedPositions)
    val sdsd = math.sqrt(variance(detectedPositions) / res.length)

    new PowerAndSD(sd, power, sdsd)
  }
}


/**
  * Created by buzun on 22/11/16.
  */
object QualityTest extends App {
  def exec(windowSize : Int, patternFactory: PatternFactory, datasetGenerator : DatasetGenerator) = {

    val model = new NormalModelVecMean(dim)

    val data = datasetGenerator.apply()

    val detector = new LRTOfflineDetector(model, 0.05, Array(windowSize), patternFactory)
    detector.init(data)

    detector.findOne(data)
  }

  val n = 500
  val dim = 5
  val dm = 0.3
  private val windowSizes = 40 to 100 by 20

  private def getPowerAndSD(iterations: Int)(patternFactory: PatternFactory)(windowSize: Int) = PowerAndSD(n - 1 - windowSize)((1 to iterations).map(_ => exec(windowSize, patternFactory, DatasetWithCPGenerator.apply(dim, windowSize + 1, n, dm))))

  val getPowerAndSD300 :  (PatternFactory) => (Int) => PowerAndSD = getPowerAndSD(1)
  println("POWER")
  println("NoPattern")
  windowSizes map getPowerAndSD300(NoPattern) foreach println

  println("HALFPattern")
  windowSizes map getPowerAndSD300(TrickyHalfTrianglePattern) foreach println
  //  println("Pattern")
  //  List(7, 15, 30, 60, 120).map(getPowerAndSD300(StaticTrianglePattern)).foreach(println)
//  println("Adaptive Pattern")
//  List(50, 70, 85).map(getPowerAndSD300(TrianglePattern)).foreach(println)
}
