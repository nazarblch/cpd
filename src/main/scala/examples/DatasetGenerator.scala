package examples

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.MultivariateGaussian
import datasets.DenseVectorDataset
import datasets.Dataset._

/**
  * Created by valerij on 12/2/16.
  */

trait DatasetGenerator {
  def apply(): DenseVectorDataset
}

class DatasetWithCPGenerator(private val multivariateGaussian: MultivariateGaussian,
                             private val dim : Int,
                             private val cpPositionFromEnd : Int,
                             private val length : Int,
                             private val dm : Double) extends DatasetGenerator {
  override def apply(): DenseVectorDataset = {
    multivariateGaussian.sample(length - cpPositionFromEnd) ++ multivariateGaussian.sample(cpPositionFromEnd).mapV(_ + DenseVector.fill(dim)(dm))
  }
}

object DatasetWithCPGenerator {
  def apply(dim: Int,
            cpPositionFromRight: Int,
            length: Int,
            dm: Double) = new DatasetWithCPGenerator(MultivariateGaussian(DenseVector.zeros[Double](dim), DenseMatrix.eye[Double](dim)),
                                                                    dim,
                                                                    cpPositionFromRight,
                                                                    length,
                                                                    dm)
}


class NormalDatasetGenerator(private val multivariateGaussian: MultivariateGaussian,
                             private val dim : Int,
                             private val length : Int) extends DatasetGenerator {
  def apply(): DenseVectorDataset = multivariateGaussian.sample(length)
}

object NormalDatasetGenerator {
  def apply(dim : Int, length : Int) = new NormalDatasetGenerator(MultivariateGaussian(DenseVector.zeros[Double](dim), DenseMatrix.eye[Double](dim)), dim, length)
}