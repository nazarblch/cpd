package cp_detectors

import datasets.CellT.TCellDouble
import datasets.Dataset


trait OfflineChangePointDetector[T >: TCellDouble] {

  def init(dataset: Dataset[T]): Unit

  def findAllWithScore: IndexedSeq[(Int, Double)]

  def findAll: IndexedSeq[Int] = findAllWithScore.map(_._1)

}
