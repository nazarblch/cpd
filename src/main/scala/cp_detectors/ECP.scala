package cp_detectors

import breeze.linalg.DenseVector
import datasets.DenseVectorDataset
import org.ddahl.jvmr.RInScala

/**
  * Created by valerij on 12/9/16.
  */
class ECP extends OfflineChangePointDetector[DenseVector[Double], DenseVectorDataset]{
  private val R = RInScala()

  R.eval("library(ecp)")

  override def init(dataset: DenseVectorDataset): Unit = { }

  override def findAll(dataset: DenseVectorDataset): IndexedSeq[Int] = {
    R.update("data", dataset.data.map(_.toArray))
    val cps = R.toVector[Int]("e.divisive(data, sig.lvl = 0.05, alpha = 2, R = 499)$cluster")
    cps.sliding(2).zipWithIndex filter { case(Array(first, second), position) => first != second } map(_._2) toIndexedSeq
  }

  override def name: String = "ecp"
}
