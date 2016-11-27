package cp_detectors

import datasets.CellT.TCellDouble
import datasets.Dataset


trait OfflineChangePointDetector[Row, Self <: Dataset[Row, Self]] extends ChangePointDetector[Row, Self] {

  def findAll(dataset: Self): IndexedSeq[Int]

}


trait RecurrentOfflineChangePointDetector[Row, Self <: Dataset[Row, Self]] extends OfflineChangePointDetector[Row, Self] {

  def findOne(dataset: Self): Option[Int]

  def minWindowSize: Int

  override def findAll(dataset: Self): IndexedSeq[Int] = {
    val cp_index = findOne(dataset)
    if (cp_index.isDefined) {
      findAll(dataset.subset(0, cp_index.get + minWindowSize)) ++
        IndexedSeq(cp_index.get) ++
        findAll(dataset.subset(cp_index.get - minWindowSize, dataset.size))
    } else {
      IndexedSeq()
    }
  }

}
