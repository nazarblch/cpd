package cp_detectors

import datasets.CellT.TCellDouble
import datasets.Dataset

import scala.util.Random


trait OnlineChangePointDetector[Row, Self] extends ChangePointDetector[Row, Self] {

  //def addData(dataset: Dataset[Row, Self]): Unit = dataset.getRowsIterator.foreach(addData)

  def addData(row: Row): Unit

  def hasNewChangePoint: Boolean

  def clear(): Unit
}


object SimpleOnlineChangePointDetector extends OnlineChangePointDetector[Double, Any] {

  val Pr: Double = 0.4
  val r = new Random()

  override def addData(row: Double): Unit = {}

  override def hasNewChangePoint: Boolean = r.nextDouble() < Pr



  override def name: String = "Simple"

  override def clear(): Unit = {}

  override def init(dataset: Any): Unit = {}
}


