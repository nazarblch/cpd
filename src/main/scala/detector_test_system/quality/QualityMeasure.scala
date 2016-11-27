package detector_test_system.quality

import breeze.numerics.log2
import datasets.CellT.TCellDouble
import datasets.Dataset
import breeze.stats.mean


import scala.collection.immutable.Set
import scala.collection.mutable.ArrayBuffer


trait QualityMeasure[Row, Self] {

  def addObservation(predictions: IndexedSeq[Int], reference: IndexedSeq[Int], data: Dataset[Row, Self]): Unit

  def getScore: Option[Double]

  def name: String

}

object QualityMeasures {

  val PRECISION = "Precision"
  val RECALL = "Recall"
  val DELAY = "Delay"
  val NMI = "NMI"

  def createMeasure[Row, Self](name: String): QualityMeasure[Row, Self]  = name match {
    case PRECISION => new Precision
    case RECALL => new Recall
    case DELAY => new AverageDelay
    case NMI => new NMI
  }

  def apply[Row, Self](names: Set[String]): Set[QualityMeasure[Row, Self]] = {
    names.map(createMeasure[Row, Self])
  }
}

abstract class PrecisionRecall[Row, Self] extends QualityMeasure[Row, Self] {

  var selected: Int = 0
  var relevant: Int = 0
  var intersected: Int = 0
  var updatesCount: Int = 0

  override def addObservation(predictions: IndexedSeq[Int], reference: IndexedSeq[Int], data: Dataset[Row, Self]) {
    assert(predictions.distinct.length == predictions.length)
    assert(reference.distinct.length == reference.length)
    selected += predictions.length
    relevant += reference.length
    intersected += findIntersection(predictions, reference)
    updatesCount += 1
  }


  def findIntersection(predictions: IndexedSeq[Int], reference: IndexedSeq[Int]): Int = {
    val refWithInf = reference :+ Int.MaxValue
    (0 until reference.length).count(i => {
      val left = refWithInf(i)
      val right = refWithInf(i + 1)
      !predictions.forall(j => j <= left || j > right)
    })
  }
}


class Precision[Row, Self] extends PrecisionRecall[Row, Self] {

  override def getScore: Option[Double] = {
    val res = if (selected == 0) 1.0 else  intersected.toDouble / selected
    if (updatesCount == 0) None else Some(res)
  }

  override def name: String = QualityMeasures.PRECISION
}

class Recall[Row, Self] extends PrecisionRecall[Row, Self] {

  override def getScore: Option[Double] = {
    val res = if (relevant == 0) 1.0 else  intersected.toDouble / relevant
    if (updatesCount == 0) None else Some(res)
  }

  override def name: String = QualityMeasures.RECALL
}

class AverageDelay[Row, Self] extends QualityMeasure[Row, Self] {

  val buf: ArrayBuffer[Double] = ArrayBuffer()

  override def addObservation(predictions: IndexedSeq[Int], reference: IndexedSeq[Int], data: Dataset[Row, Self]): Unit = {
    val refWithInf = reference :+ Int.MaxValue
    (0 until reference.length).foreach(i => {
      val left = refWithInf(i)
      val right = refWithInf(i + 1)
      val insideInterval = predictions.filter(j => j > left && j <= right)
      if (insideInterval.nonEmpty) {
        buf += insideInterval.min - left
      }
    })
    println("pred = " + predictions.mkString(","), "| ref = " + reference.mkString(","))
  }

  override def getScore: Option[Double] = {
    if (buf.length == 0) None else Some(mean(buf.toArray))
  }

  override def name: String = QualityMeasures.DELAY
}

class NMI[Row, Self] extends QualityMeasure[Row, Self] {

  val buf: ArrayBuffer[Double] = ArrayBuffer()

  def format(x: IndexedSeq[Int], size: Int): IndexedSeq[Int] = {
    val res = Array.fill[Int](size)(1)
    var label = 1
    x.sorted.foreach(xi => {
      label += 1
      for (i <- xi until size) {
        res(i) = label
      }
    })
    res.toIndexedSeq
  }

  def mutual_info(x: IndexedSeq[Int],y: IndexedSeq[Int]): Double = {
    val N: Double = x.size
    var I=0.0
    for (l1 <- x.distinct; l2 <- y.distinct) {
      val l1_ids: Set[Int] = x.zipWithIndex.filter({case(xi, i) => xi == l1}).map(_._2).toSet
      val l2_ids: Set[Int] = y.zipWithIndex.filter({case(yi, i) => yi == l2}).map(_._2).toSet

      val pxy = l1_ids.intersect(l2_ids).size.toDouble / N

      if (pxy > 1e-8) I += pxy * log2( pxy/((l1_ids.size/N)*(l2_ids.size/N)))
    }
    I
  }

  def nmi(x: IndexedSeq[Int],y: IndexedSeq[Int]): Double = {
    val N: Double = x.size
    val I=mutual_info(x,y)

    var Hx = 0.0

    for (l1 <- x.distinct) {
      val l1_ids: Set[Int] = x.zipWithIndex.filter({case(xi, i) => xi == l1}).map(_._2).toSet
      Hx += -(l1_ids.size.toDouble / N)* log2(l1_ids.size.toDouble / N)
    }

    var Hy = 0.0

    for (l2 <- y.distinct) {
      val l2_ids: Set[Int] = y.zipWithIndex.filter({case(yi, i) => yi == l2}).map(_._2).toSet
      Hy += -(l2_ids.size.toDouble / N)* log2(l2_ids.size.toDouble / N)
    }

    if(Hx < 1e-8 && Hy < 1e-8) 1.0
    else 2 * I / (Hx + Hy)
  }


  override def addObservation(predictions: IndexedSeq[Int], reference: IndexedSeq[Int], data: Dataset[Row, Self]): Unit = {
    val pred = format(predictions, data.size)
    val ref = format(reference, data.size)
    buf += nmi(pred, ref)
    println("pred = " + predictions.mkString(","), "| ref = " + reference.mkString(","))
    println("NMIs =" +  buf.mkString(","))
  }

  override def getScore: Option[Double] = {
    if (buf.length == 0) None else Some(mean(buf.toArray))
  }

  override def name: String = QualityMeasures.NMI
}