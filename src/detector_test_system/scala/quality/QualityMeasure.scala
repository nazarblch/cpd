package quality

import breeze.numerics.log2
import datasets.CellT.TCellDouble
import datasets.Dataset
import breeze.stats.mean
import jep.Jep


import scala.collection.immutable.Set
import scala.collection.mutable.ArrayBuffer


trait QualityMeasure[T >: TCellDouble] {

  def addObservation(predictions: IndexedSeq[Int], reference: IndexedSeq[Int], data: Dataset[T]): Unit

  def getScore: Option[Double]

  def name: String

}

object QualityMeasures {

  val PRECISION = "Precision"
  val RECALL = "Recall"
  val DELAY = "Delay"
  val NMI = "NMI"

  def createMeasure[T >: TCellDouble](name: String): QualityMeasure[T]  = name match {
    case PRECISION => new Precision
    case RECALL => new Recall
    case DELAY => new AverageDelay
    case NMI => new NMI
  }

  def apply[T >: TCellDouble](names: Set[String]): Set[QualityMeasure[T]] = {
    names.map(createMeasure[T])
  }
}

abstract class PrecisionRecall[T >: TCellDouble] extends QualityMeasure[T] {

  var selected: Int = 0
  var relevant: Int = 0
  var intersected: Int = 0
  var updatesCount: Int = 0

  override def addObservation(predictions: IndexedSeq[Int], reference: IndexedSeq[Int], data: Dataset[T]) {
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


class Precision[T >: TCellDouble] extends PrecisionRecall[T] {

  override def getScore: Option[Double] = {
    val res = if (selected == 0) 1.0 else  intersected.toDouble / selected
    if (updatesCount == 0) None else Some(res)
  }

  override def name: String = QualityMeasures.PRECISION
}

class Recall[T >: TCellDouble] extends PrecisionRecall[T] {

  override def getScore: Option[Double] = {
    val res = if (relevant == 0) 1.0 else  intersected.toDouble / relevant
    if (updatesCount == 0) None else Some(res)
  }

  override def name: String = QualityMeasures.RECALL
}

class AverageDelay[T >: TCellDouble] extends QualityMeasure[T] {

  val buf: ArrayBuffer[Double] = ArrayBuffer()

  override def addObservation(predictions: IndexedSeq[Int], reference: IndexedSeq[Int], data: Dataset[T]): Unit = {
    val refWithInf = reference :+ Int.MaxValue
    (0 until reference.length).foreach(i => {
      val left = refWithInf(i)
      val right = refWithInf(i + 1)
      val insideInterval = predictions.filter(j => j > left && j <= right)
      if (insideInterval.nonEmpty) {
        buf += insideInterval.min - left
      }
    })
  }

  override def getScore: Option[Double] = {
    if (buf.length == 0) None else Some(mean(buf.toArray))
  }

  override def name: String = QualityMeasures.DELAY
}

class NMI[T >: TCellDouble] extends QualityMeasure[T] {

  def mutual_info(x: IndexedSeq[Int],y: IndexedSeq[Int]): Double = {
    val N: Double = x.size
    var I=0.0
    val eps = 1e-5
    for (l1 <- x.distinct; l2 <- y.distinct) {
      val l1_ids: Set[Int] = x.zipWithIndex.filter({case(xi, i) => xi == l1}).map(_._2).toSet
      val l2_ids: Set[Int] = y.zipWithIndex.filter({case(yi, i) => yi == l2}).map(_._2).toSet

      val pxy = l1_ids.intersect(l2_ids).size.toDouble / N + eps
      I += pxy * log2( pxy/((l1_ids.size/N)*(l2_ids.size/N)))
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

    I/((Hx+Hy)/2)
  }


  override def addObservation(predictions: IndexedSeq[Int], reference: IndexedSeq[Int], data: Dataset[T]): Unit = {

  }

  override def getScore: Option[Double] = Some(1.0)

  override def name: String = QualityMeasures.NMI
}