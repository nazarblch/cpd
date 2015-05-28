package cp_detectors

import datasets.{OneColumnDataset, Dataset}
import org.ddahl.jvmr.RInScala


class CUSUMOfflineDetector extends OfflineChangePointDetector[Double, OneColumnDataset[Double]] {

  val R = RInScala()

  def importRLib(path: String) {
    R.eval(io.Source.fromFile(path).getLines().mkString("\n"))
  }

  importRLib("src/R/cusum.r")

  override def findAll(dataset: OneColumnDataset[Double]): IndexedSeq[Int] = {

    assert(dataset.dim == 1)

    R.update("data", dataset.data.data.toArray)

    var res: Array[Int] = Array()


    res = R.toVector[Double]("cusum(data)").map(_.toInt - 1)


    println(res.toList)

    res.toIndexedSeq
  }

  override def init(dataset: OneColumnDataset[Double]): Unit = {}

  override def name: String = "CUSUM"
}
