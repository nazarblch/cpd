package quality

import datasets.CellT.TCellDouble
import datasets.Dataset


trait QualityMeasure[T >: TCellDouble] {

  def addObservation(predictions: IndexedSeq[Int], reference: IndexedSeq[Int], data: Dataset[T]): Double

  def getScore: Double

  def name: String

}
