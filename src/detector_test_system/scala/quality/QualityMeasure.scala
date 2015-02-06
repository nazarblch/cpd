package quality

import datasets.CellT.TCellDouble
import datasets.Dataset
import breeze.stats.mean

import scala.collection.mutable.ArrayBuffer


trait QualityMeasure[T >: TCellDouble] {

  def addObservation(predictions: IndexedSeq[Int], reference: IndexedSeq[Int], data: Dataset[T]): Unit

  def getScore: Option[Double]

  def name: String

}

object QualityMeasures {

  def createMeasure[T >: TCellDouble](name: String): QualityMeasure[T]  = name match {
    case "Precision" => new Precision
    case "Recall" => new Recall
    case "Delay" => new AverageDelay
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

  override def name: String = "Precision"
}

class Recall[T >: TCellDouble] extends PrecisionRecall[T] {

  override def getScore: Option[Double] = {
    val res = if (relevant == 0) 1.0 else  intersected.toDouble / relevant
    if (updatesCount == 0) None else Some(res)
  }

  override def name: String = "Recall"
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

  override def name: String = "Delay"
}