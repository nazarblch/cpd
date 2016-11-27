package examples

import datasets._
import utils.Tabulator

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







}
