import datasets.CellT.CellType
import datasets._
import utils.Tabulator


import scala.collection.IndexedSeq

object LoadData extends App {


  def getHeaderSeq[T >: CellType with Double](dataset: Dataset[T]): IndexedSeq[String] = dataset.isNumeric match {
    case true => dataset.header.data
    case false => dataset.header.data.zip(dataset.getRow(0).map(_.asInstanceOf[CellType].getType)).map{case (name,t) => name + ":" + t}
  }

  def print[T >: CellType with Double](dataset: Dataset[T]): Unit = {
    println( Tabulator.format(Seq(
      getHeaderSeq(dataset)
    ) ++
    dataset.getRowsIterator
    ))
  }

  val data: Dataset[CellType] = DatasetLoader.loadFromFile("data/data.csv")

  print(data)


  val data1: Dataset[Double] = DatasetConverter.toNumeric(DatasetLoader.loadFromFile("data/numeric.csv"))

  print(data1)


}
