package examples

import breeze.linalg._
import datasets.Dataset._
import breeze.stats._
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.MultivariateGaussian
import cp_detectors.LRTOfflineDetector
import datasets.{Dataset, DenseVectorDataset, MultiColumnDataset}
import models.standart.NormalModelVecMean
import patterns.{NoPattern, PatternFactory, StaticTrianglePattern, TrianglePattern}

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

  private def generateDataWithCP(r : MultivariateGaussian, dim : Int, n : Int, dm: Double): DenseVectorDataset = {
      r.sample(n / 2).toIndexedSeq ++
        r.sample(n / 2).toIndexedSeq.map(x => x + DenseVector.fill(dim)(dm))
  }

  def exec(windowSize : Int, patternFactory: PatternFactory) = {
    val dim = 30
    val model = new NormalModelVecMean(dim)

    val r = MultivariateGaussian(DenseVector.zeros[Double](dim), DenseMatrix.eye[Double](dim))

    val dm = 0.2
    val n = 500

    val data = generateDataWithCP(r, dim, n, dm)

    val detector = new LRTOfflineDetector(model, 0.05, Array(windowSize), patternFactory)
    detector.init(data)

    detector.findOne(data)
  }


  private def getPowerAndSD(iterations: Int)(patternFactory: PatternFactory)(windowSize: Int) = PowerAndSD((1 to iterations).map(_ => exec(windowSize, patternFactory)))

  val getPowerAndSD300 :  (PatternFactory) => (Int) => PowerAndSD = getPowerAndSD(300)

//  println("NoPattern")
//  List(25, 35, 50, 70, 85, 100).map(getPowerAndSD300(NoPattern)).foreach(println)
  println("Adaptive Pattern")
  List(50, 70, 85).map(getPowerAndSD300(TrianglePattern)).foreach(println)
}
