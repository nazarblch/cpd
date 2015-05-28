import datasets.CellT.CellType
import datasets._
import utils.Tabulator


import scala.collection.IndexedSeq

object LoadData extends App {


  def getHeaderSeq[Row, Self <: Dataset[Row, Self]](dataset: Self): IndexedSeq[String] = dataset.isNumeric match {
    case true => dataset.header.data
    case false => dataset.header.data
  }

  def print[Row, Self <: Dataset[Row, Self]](dataset: Self): Unit = {
    println( Tabulator.format(Seq(
      getHeaderSeq[Row, Self](dataset)
    ) ++
    dataset.toMatrix
    ))
  }

  val data = DatasetLoader.loadFromFile("data/data.csv")

  print[CellType, OneColumnDataset[CellType]](data)


  val data1 = DatasetConverter.toNumeric(DatasetLoader.loadFromFile("data/numeric.csv"))

  print[Double, OneColumnDataset[Double]](data1)


}
