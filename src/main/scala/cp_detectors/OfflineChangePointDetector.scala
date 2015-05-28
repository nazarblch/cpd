package cp_detectors

import datasets.CellT.TCellDouble
import datasets.Dataset


trait OfflineChangePointDetector[Row, Self] extends ChangePointDetector[Row, Self] {

  def findAll(dataset: Self): IndexedSeq[Int]

}
