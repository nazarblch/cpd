package cp_detectors

import datasets.{OneColumnDataset, Dataset}
import org.ddahl.jvmr.RInScala


class MeanVarOfflineDetector extends OfflineChangePointDetector[Double, OneColumnDataset[Double]] {

  val R = RInScala()

  def importRLib(path: String) {
    R.eval(io.Source.fromFile(path).getLines().mkString("\n"))
  }

  importRLib("src/R/meanvar.r")

  override def findAll(dataset: OneColumnDataset[Double]): IndexedSeq[Int] = {

    assert(dataset.dim == 1)

    R.update("data", dataset.data.data.toArray)

    R.toVector[Int]("cpLocations(data, 'PELT', 'Normal')").map(_ - 1)
  }

  override def init(dataset: OneColumnDataset[Double]): Unit = {}

  override def name: String = "RMeanVar"
}
