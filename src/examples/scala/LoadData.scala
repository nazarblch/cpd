import java.util

import breeze.linalg.DenseVector
import datasets.CellT.CellType
import datasets._
import edu.uci.lasso.{LassoFit, LassoFitGenerator}
import jml.options.Options
import jml.regression.LASSO
import models.standart.{RegressionModelWithL1, RegressionModel}
import utils.Tabulator


import scala.collection.IndexedSeq
import scala.util.Random

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




  val data1 = DatasetConverter.toNumeric(DatasetLoader.loadFromFileMulticol("data/numeric.csv"))

  // print[Vector[Double], MultiColumnDataset[Double]](data1)

  val model = new RegressionModel(data1.dim - 1, data1.dim - 1)
  val modelL1 = new RegressionModelWithL1(data1.dim - 1, data1.dim - 1, data1.header)
  val mle = modelL1.MLE(data1)

  println(mle)
  println(modelL1.likelihood(data1))


}
