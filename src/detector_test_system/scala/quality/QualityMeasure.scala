package quality

import datasets.CellT.TCellDouble
import datasets.Dataset


trait QualityMeasure[T >: TCellDouble] {

  def score(predictions: IndexedSeq[Int], reference: IndexedSeq[Int]): Double

}
