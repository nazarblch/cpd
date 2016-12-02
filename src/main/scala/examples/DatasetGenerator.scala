package examples

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.MultivariateGaussian
import datasets.DenseVectorDataset

/**
  * Created by valerij on 12/2/16.
  */

trait DatasetGenerator {
  def apply(n : Int) : DenseVectorDataset
}

class DatasetWithCPGenerator(private val datasetGenerator: NormalDatasetGenerator,
                             private val dim : Int,
                             private val dm : Double) extends DatasetGenerator {
  override def apply(n : Int): DenseVectorDataset = {
    datasetGenerator(n / 2) ++ datasetGenerator(n / 2).mapV(_ + DenseVector.fill(dim)(dm))
  }
}

object DatasetWithCPGenerator {
  def apply(dim : Int, dm : Double) = new DatasetWithCPGenerator(NormalDatasetGenerator(dim), dim, dm)
}


class NormalDatasetGenerator(private val multivariateGaussian: MultivariateGaussian, private val dim : Int) extends DatasetGenerator {
  def apply(n : Int) : DenseVectorDataset = multivariateGaussian.sample(n)
}

object NormalDatasetGenerator {
  def apply(dim : Int) = new NormalDatasetGenerator(MultivariateGaussian(DenseVector.zeros[Double](dim), DenseMatrix.eye[Double](dim)), dim)
}