package cp_detectors

import _root_.addData.BOCP
import breeze.linalg.{DenseVector, DenseMatrix}
import com.mathworks.toolbox.javabuilder.MWClassID
import com.mathworks.toolbox.javabuilder.MWNumericArray
import datasets.Dataset

import scala.collection.mutable.ArrayBuffer


class D1BayesianOnlineCPDetector extends OnlineChangePointDetector[Double] {

  val MAX_TIME = 1000
  val bocp = new BOCP()

  var data: Array[Option[Double]] = Array.fill[Option[Double]](MAX_TIME)(None)
  var R: DenseMatrix[Double] = DenseMatrix.zeros[Double](MAX_TIME, MAX_TIME)
  R(0,0) = 1.0
  var maxes: Array[Int] = Array.fill(MAX_TIME)(0)
  var size = 0

  var muT    = new MWNumericArray(0.0, MWClassID.DOUBLE)
  var kappaT = new MWNumericArray(1.0, MWClassID.DOUBLE)
  var alphaT = new MWNumericArray(1.0, MWClassID.DOUBLE)
  var betaT  = new MWNumericArray(1.0, MWClassID.DOUBLE)
  var Rt: MWNumericArray = new MWNumericArray(1.0, MWClassID.DOUBLE)


  override def addData(row: IndexedSeq[Double]): Unit = {

    assert(row.length == 1)

    data(size) = Some(row(0))

    val X: Double = row(0)
    val t: Int = size + 1


    val result: Array[Object] = bocp.addData(5, X.asInstanceOf[Object], t.asInstanceOf[Object], muT, kappaT, alphaT, betaT, Rt)

    (0 to size + 1).foreach(i => {
      R(i, size + 1) = result(0).asInstanceOf[MWNumericArray].toArray.asInstanceOf[Array[Array[Double]]](i)(0)
    })

    Rt    = result(0).asInstanceOf[MWNumericArray]
    muT    = result(1).asInstanceOf[MWNumericArray]
    kappaT = result(2).asInstanceOf[MWNumericArray]
    alphaT = result(3).asInstanceOf[MWNumericArray]
    betaT  = result(4).asInstanceOf[MWNumericArray]

    maxes(size+1) = R(0 to size+1, size+1).toScalaVector().zipWithIndex.maxBy(_._1)._2

    size += 1
  }

  val WH = 3

  override def hasNewChangePoint: Boolean = {
    (size > 2 * WH) && (maxes.slice(size - WH, size).max < 0.5 * maxes(size - WH - 1))
  }

  override def clear(): Unit = {
    data = Array.fill[Option[Double]](MAX_TIME)(None)
    R = DenseMatrix.zeros[Double](MAX_TIME, MAX_TIME)
    R(0,0) = 1.0
    maxes = Array.fill(MAX_TIME)(0)
    size = 0

    muT    = new MWNumericArray(Array(0.0), MWClassID.DOUBLE)
    kappaT = new MWNumericArray(Array(1.0), MWClassID.DOUBLE)
    alphaT = new MWNumericArray(Array(1.0), MWClassID.DOUBLE)
    betaT  = new MWNumericArray(Array(1.0), MWClassID.DOUBLE)
  }

  override def init(dataset: Dataset[Double]): Unit = {}

  override def name: String = "BOCPD"
}
