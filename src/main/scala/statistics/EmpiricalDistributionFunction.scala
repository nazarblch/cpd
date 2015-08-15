package statistics

import breeze.linalg.DenseVector
import datasets.CellT.{TCellDouble, CellType}
import datasets._
import Ordering.Implicits._
import scala.collection.parallel.immutable.ParVector


object EmpiricalDistributionFunction {

  // fixme: implement tree structure for speedup

  def value(column: Column[Double], x: Double): Double = {
    column.data.partition(_ <  x)._1.length.toDouble / column.data.length
  }



  def value(column: Column[Double], x: Double, weights: Vector[Double]): Double = {
    column.data.zip(weights).partition(_._1 <  x)._1.map(_._2).sum / weights.sum
  }


  def value[Row, Self <: Dataset[Row, Self]](dataset: WeightedDataset[Row, Self], x: IndexedSeq[Double]): Double = {
    dataset.toDataset.getColumns.zipWithIndex.map{case(col, i) => value(col, x(i), dataset.weights)}.product
  }

  def value[Row, Self <: Dataset[Row, Self]](dataset: Self, x: IndexedSeq[Double]): Double = {
    dataset.getColumns.zipWithIndex.map{case(col, i) => value(col, x(i))}.product
  }




}
