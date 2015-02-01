package cp_detectors

import datasets.CellT.TCellDouble
import datasets.Dataset


trait OnlineChangePointDetector[T >: TCellDouble] {

  def init(dataset: Dataset[T]): Unit

  def addData(dataset: Dataset[T]): Unit

  def addData(row: IndexedSeq[T]): Unit

  def hasNewChangePoint: Boolean

  def findLast: Int
}
