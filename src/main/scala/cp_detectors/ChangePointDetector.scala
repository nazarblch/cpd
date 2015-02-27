package cp_detectors

import datasets.CellT.TCellDouble
import datasets.Dataset


trait ChangePointDetector[T >: TCellDouble] {

  def params: Map[String, String with Double] = Map()

  def init(dataset: Dataset[T]): Unit

  def name: String

}
