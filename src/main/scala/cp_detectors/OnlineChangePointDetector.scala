package cp_detectors

import datasets.CellT.TCellDouble
import datasets.Dataset

import scala.util.Random


trait OnlineChangePointDetector[T >: TCellDouble] extends ChangePointDetector[T] {

  def addData(dataset: Dataset[T]): Unit = dataset.getRowsIterator.foreach(addData)

  def addData(row: IndexedSeq[T]): Unit

  def hasNewChangePoint: Boolean

}


object SimpleOnlineChangePointDetector extends OnlineChangePointDetector[Double] {

  val Pr: Double = 0.4
  val r = new Random()

  override def addData(row: IndexedSeq[Double]): Unit = {}

  override def hasNewChangePoint: Boolean = r.nextDouble() < Pr

  override def init(dataset: Dataset[Double]): Unit = {}

  override def name: String = "Simple"
}


