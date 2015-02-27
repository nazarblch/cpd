package cp_detectors

import datasets.Dataset
import org.ddahl.jvmr.RInScala


class CUSUMOfflineDetector extends OfflineChangePointDetector[Double] {

  val R = RInScala()

  def importRLib(path: String) {
    R.eval(io.Source.fromFile(path).getLines().mkString("\n"))
  }

  importRLib("src/R/cusum.r")

  override def findAll(dataset: Dataset[Double]): IndexedSeq[Int] = {

    assert(dataset.dim == 1)

    R.update("data", dataset.data(0).data.toArray)

    var res: Array[Int] = Array()


    res = R.toVector[Double]("cusum(data)").map(_.toInt - 1)


    println(res.toList)

    res.toIndexedSeq
  }

  override def init(dataset: Dataset[Double]): Unit = {}

  override def name: String = "CUSUM"
}
