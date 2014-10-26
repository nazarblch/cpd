import datasets.CellT.CellType
import datasets._
import utils.Tabulator

object LoadData extends App {

  def print(dataset: Dataset[CellType]): Unit = {
    println( Tabulator.format(Seq(
      dataset.header.zip(dataset.getRow(0).map(_.getType)).map{case (name,t) => name + ":" + t}
    ) ++
    dataset.getRowsIterator
    ))
  }

  def print(dataset: NumericDataset): Unit = {
    println( Tabulator.format(Seq(
      dataset.header
    ) ++
      dataset.getRowsIterator
    ))
  }

  val data: CellTDataset = DatasetLoader.loadFromFile("data/data.csv")

  print(data)


  val data1: NumericDataset = DatasetLoader.loadFromFile("data/numeric.csv").toNumeric

  print(data1)


}
