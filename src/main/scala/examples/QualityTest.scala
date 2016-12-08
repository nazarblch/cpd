package examples

import breeze.linalg._
import datasets.Dataset._
import breeze.stats._
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.MultivariateGaussian
import com.jhlabs.image.HalftoneFilter
import cp_detectors.LRTOfflineDetector
import datasets.{Dataset, DenseVectorDataset, MultiColumnDataset}
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
  def apply(res : Seq[Option[Int]]) = {
    val power = res.count(_.isDefined).toDouble / res.length
    val detectedPositions = res.flatten.map(x => math.abs(x - 250).toDouble)
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

    val n = 500

    val data = datasetGenerator(n)

    val detector = new LRTOfflineDetector(model, 0.05, Array(windowSize), patternFactory)
    detector.init(data)

    detector.findOne(data)
  }

  val dim = 5
  val dm = 0.2
  val dataGenerator = DatasetWithCPGenerator(dim, dm)

  private def getPowerAndSD(iterations: Int)(patternFactory: PatternFactory)(windowSize: Int) = PowerAndSD((1 to iterations).map(_ => exec(windowSize, patternFactory, dataGenerator)))

  val getPowerAndSD300 :  (PatternFactory) => (Int) => PowerAndSD = getPowerAndSD(300)

//  println("POWER")
//  println("NoPattern")
//  List(7, 15, 30, 60, 120).map(getPowerAndSD300(NoPattern)).foreach(println)
//
//  println("Pattern")
//  List(7, 15, 30, 60, 120).map(getPowerAndSD300(StaticTrianglePattern)).foreach(println)

  println("HALFPattern")
  List(80,120, 160,200,240).map(getPowerAndSD300(StaticHalfTrianglePattern)).foreach(println)
//  println("Adaptive Pattern")
//  List(50, 70, 85).map(getPowerAndSD300(TrianglePattern)).foreach(println)
}
