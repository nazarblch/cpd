package statistics

import breeze.linalg.DenseVector
import datasets.CellT.CellType
import datasets._
import Ordering.Implicits._
import scala.collection.parallel.immutable.ParVector


object EmpiricalDistributionFunction {

  // fixme: implement tree structure for speedup

  def value(column: Column[DoubleCellT], x: DoubleCellT): Double = {
    column.data.partition(_ <  x)._1.length.toDouble / column.data.length
  }

  def value(column: Column[IntCellT], x: IntCellT): Double = {
    column.data.partition(_ <  x)._1.length.toDouble / column.data.length
  }

  def value(column: Column[CatCellT], x: CatCellT): Double = {
    column.data.partition(_ <  x)._1.length.toDouble / column.data.length
  }

  def value(column: Column[DoubleCellT], x: DoubleCellT, weights: ParVector[Double]): Double = {
    column.data.zip(weights).partition(_._1 <  x)._1.map(_._2).sum / weights.sum
  }

  def value(column: Column[IntCellT], x: IntCellT, weights: ParVector[Double]): Double = {
    column.data.zip(weights).partition(_._1 <  x)._1.map(_._2).sum / weights.sum
  }

  def value(column: Column[CatCellT], x: CatCellT, weights: ParVector[Double]): Double = {
    column.data.zip(weights).partition(_._1 <  x)._1.map(_._2).sum / weights.sum
  }


  def value(column: Column[CellType], x: CellType): Double = {
    if (column(0).getType equals CellT.DOUBLE_TYPE_NAME) {
      value(column.asInstanceOf[Column[DoubleCellT]], x.asInstanceOf[DoubleCellT])
    } else if (column(0).getType equals CellT.INT_TYPE_NAME) {
      value(column.asInstanceOf[Column[IntCellT]], x.asInstanceOf[IntCellT])
    } else {
      value(column.asInstanceOf[Column[CatCellT]], x.asInstanceOf[CatCellT])
    }
  }


  def value(column: Column[CellType], x: CellType, weights: ParVector[Double]): Double = {
    if (column(0).getType equals CellT.DOUBLE_TYPE_NAME) {
      value(column.asInstanceOf[Column[DoubleCellT]], x.asInstanceOf[DoubleCellT], weights)
    } else if (column(0).getType equals CellT.INT_TYPE_NAME) {
      value(column.asInstanceOf[Column[IntCellT]], x.asInstanceOf[IntCellT], weights)
    } else {
      value(column.asInstanceOf[Column[CatCellT]], x.asInstanceOf[CatCellT], weights)
    }
  }


  def value(dataset: WeightedDataset[CellType], x: IndexedSeq[CellType]): Double = {
    dataset.data.zipWithIndex.map{case(col, i) => value(col, x(i), dataset.weights)}.product
  }

  def value(dataset: Dataset[CellType], x: IndexedSeq[CellType]): Double = {
    dataset.data.zipWithIndex.map{case(col, i) => value(col, x(i))}.product
  }




}
