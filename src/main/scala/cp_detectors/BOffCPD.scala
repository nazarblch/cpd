//package cp_detectors
//
//import addData.BOCP
//import breeze.linalg.DenseMatrix
//import com.mathworks.toolbox.javabuilder.{MWClassID, MWNumericArray}
//import datasets.{OneColumnDataset, Dataset}
//
//class BOffCPD extends OfflineChangePointDetector[Double, OneColumnDataset[Double]] {
//
//  val MAX_TIME = 1000
//  val bocp = new BOCP()
//
//  var data: Array[Option[Double]] = Array.fill[Option[Double]](MAX_TIME)(None)
//  var R: DenseMatrix[Double] = DenseMatrix.zeros[Double](MAX_TIME, MAX_TIME)
//  R(0,0) = 1.0
//  var maxes: Array[Int] = Array.fill(MAX_TIME)(0)
//  var size = 0
//
//  var muT    = new MWNumericArray(0.0, MWClassID.DOUBLE)
//  var kappaT = new MWNumericArray(1.0, MWClassID.DOUBLE)
//  var alphaT = new MWNumericArray(1.0, MWClassID.DOUBLE)
//  var betaT  = new MWNumericArray(1.0, MWClassID.DOUBLE)
//  var Rt: MWNumericArray = new MWNumericArray(1.0, MWClassID.DOUBLE)
//
//
//  def addData(row: Double): Unit = {
//
//    data(size) = Some(row)
//
//    val X: Double = row
//    val t: Int = size + 1
//
//
//    val result: Array[Object] = bocp.addData(5, X.asInstanceOf[Object], t.asInstanceOf[Object], muT, kappaT, alphaT, betaT, Rt)
//
//    (0 to size + 1).foreach(i => {
//      R(i, size + 1) = result(0).asInstanceOf[MWNumericArray].toArray.asInstanceOf[Array[Array[Double]]](i)(0)
//    })
//
//    Rt    = result(0).asInstanceOf[MWNumericArray]
//    muT    = result(1).asInstanceOf[MWNumericArray]
//    kappaT = result(2).asInstanceOf[MWNumericArray]
//    alphaT = result(3).asInstanceOf[MWNumericArray]
//    betaT  = result(4).asInstanceOf[MWNumericArray]
//
//    maxes(size+1) = R(0 to size+1, size+1).toScalaVector().zipWithIndex.maxBy(_._1)._2
//
//    size += 1
//  }
//
//  val WH = 3
//
//  def hasNewChangePoint(pos: Int): Boolean = {
//    (pos > 2 * WH) && (maxes.slice(pos - WH, pos).max < 0.5 * maxes(pos - WH - 1))
//  }
//
//  def getCP(pos: Int): Option[Int] = {
//    if (hasNewChangePoint(pos)) Some(pos - WH - 1) else None
//  }
//
//  override def findAll(dataset: OneColumnDataset[Double]): IndexedSeq[Int] = {
//    data = Array.fill[Option[Double]](MAX_TIME)(None)
//    R = DenseMatrix.zeros[Double](MAX_TIME, MAX_TIME)
//    R(0,0) = 1.0
//    maxes = Array.fill(MAX_TIME)(0)
//    size = 0
//
//    muT    = new MWNumericArray(Array(0.0), MWClassID.DOUBLE)
//    kappaT = new MWNumericArray(Array(1.0), MWClassID.DOUBLE)
//    alphaT = new MWNumericArray(Array(1.0), MWClassID.DOUBLE)
//    betaT  = new MWNumericArray(Array(1.0), MWClassID.DOUBLE)
//
//    dataset.getRowsIterator.foreach(addData)
//
//    IndexedSeq.range(0, dataset.size).flatMap(pos => getCP(pos))
//  }
//
//  override def init(dataset: OneColumnDataset[Double]): Unit = {}
//
//  override def name: String = "BOCPD"
//}