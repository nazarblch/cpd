package cp_detectors

import datasets.CellT.TCellDouble
import datasets.Dataset


trait OfflineChangePointDetector[T >: TCellDouble] extends ChangePointDetector[T] {

  def findAll: IndexedSeq[Int]

}
