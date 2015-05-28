package cp_detectors

import datasets.CellT.TCellDouble
import datasets.Dataset


trait ChangePointDetector[Row, Self] {

  def params: Map[String, String with Double] = Map()

  def init(dataset: Self): Unit

  def name: String

}
