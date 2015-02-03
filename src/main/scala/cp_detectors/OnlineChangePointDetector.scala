package cp_detectors

import datasets.CellT.TCellDouble
import datasets.Dataset


trait OnlineChangePointDetector[T >: TCellDouble] extends ChangePointDetector[T] {

  def addData(dataset: Dataset[T]): Unit = dataset.getRowsIterator.foreach(addData)

  def addData(row: IndexedSeq[T]): Unit

  def hasNewChangePoint: Boolean

}
