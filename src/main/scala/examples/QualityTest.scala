package examples

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.MultivariateGaussian
import cp_detectors.LRTOfflineDetector
import datasets.{Dataset, DenseVectorDataset, MultiColumnDataset}
import models.standart.NormalModelVecMean
import patterns.{StaticTrianglePattern, TrianglePattern}

/**
  * Created by buzun on 22/11/16.
  */
object QualityTest extends App {

  val dim = 20
  val model = new NormalModelVecMean(dim)

  // val r = new util.Random()
  val r = new MultivariateGaussian(DenseVector.zeros[Double](dim), DenseMatrix.eye[Double](dim))

  val dm = 0.10
  val n = 500

  def data_gen(dm: Double = dm): DenseVectorDataset = {
    Dataset.applyVec(
      r.sample(n/2).toIndexedSeq ++
        r.sample(n/2).toIndexedSeq.map(x => x + DenseVector.fill(dim)(dm)))
  }

  val data = data_gen(dm)

  val detector = new LRTOfflineDetector(model, 0.1, Array(80, 90, 100, 110), TrianglePattern)
  detector.init(data)
  val cp = detector.findOne(data)

  println(cp)

}
