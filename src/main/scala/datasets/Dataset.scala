package datasets

import datasets.CellT.CellType

import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.mutable.ParArray

trait ColumnT[T, C] {
  def size: Int
  def slice(from: Int, to: Int): C
  def apply(index: Int): T
  def ++ (other: C): C
  def :+ (elem: T): C
  def isNumeric: Boolean
}

class Column[T] (val data: ParArray[T]) extends ColumnT[T, Column[T]] {
  override def size = data.length
  override def slice(from: Int, to: Int): Column[T] = new Column[T](data.slice(from, to))
  override def apply(index: Int): T = data(index)
  override def ++ (other: Column[T]): Column[T] = new Column[T](data ++ other.data)
  override def :+ (elem: T): Column[T] = new Column[T](data :+ elem)
  override def isNumeric: Boolean = data(0).isInstanceOf[Double] || data(0).asInstanceOf[CellType].isNumeric
}

object Column {
  def apply[T](v: Vector[T]): Column[T] = {
    val data = ParArray.newBuilder[T]
    v.foreach(vi => data += vi)
    new Column[T](data.result())
  }
}


class Dataset[T >: DoubleCellT with IntCellT with CatCellT with Double](val header: DataHeader,
                 val data: ParVector[Column[T]],
                 val isNumeric: Boolean = false
                  ) {

  assert(data(0).size > 0)
  assert(data.forall(_.size == data(0).size))
  assert(header.size == data.length)

  def size = data(0).size

  def dim = data.size

  def getRow(index: Int): IndexedSeq[T] = {
    for (col <- 0 until header.size) yield data(col)(index)
  }

  def getRowsIterator: IndexedSeq[IndexedSeq[T]] = {
    for (i <- 0 until size) yield getRow(i)
  }

  def subset(from: Int, to: Int): Dataset[T] = {
    new Dataset[T](header, data.map(_.slice(from, to)))
  }

  def ++ (other: Dataset[T]): Dataset[T] = {
    assert(header equals other.header)
    val newData: ParVector[Column[T]] = data.zip(other.data).map({case (col1, col2) => col1 ++ col2})
    new Dataset[T](header, newData, isNumeric)
  }

  def ++ (row: IndexedSeq[T]): Dataset[T] = {
    assert(row.length equals size)
    val newData: ParVector[Column[T]] = data.zip(row).map({case (col1, elem) => col1 :+ elem})
    new Dataset[T](header, newData, isNumeric)
  }

}


object DatasetConverter {
  def toDoubleColumn(column: Column[CellType]): Column[Double] = {
    assert(column.isNumeric)
    new Column[Double](column.data.map(_.toDouble))
  }

  def toNumeric(dataset: Dataset[CellType]): Dataset[Double] = {
    new Dataset[Double](dataset.header, dataset.data.map(toDoubleColumn), true)
  }
}


class WeightedDataset[T >:  DoubleCellT with IntCellT with CatCellT with Double](override val header: DataHeader,
                                                 override val data: ParVector[Column[T]],
                                                 val weights: ParVector[Double],
                                                 override val isNumeric: Boolean = false
                                                  ) extends Dataset(header, data, isNumeric) {


  def getRowWithWeight(index: Int): (IndexedSeq[T], Double) = {
    (for (col <- 0 until header.size) yield data(col)(index), weights(index))
  }

  def getRowsWithWeightIterator: IndexedSeq[(IndexedSeq[T], Double)] = {
    for (i <- 0 until size) yield getRowWithWeight(i)
  }

  def convolution(f: (IndexedSeq[T] => Double)): Double = {
    getRowsWithWeightIterator.foldLeft(0.0)({case(res, (row,w)) => res + w * f(row)})
  }

  override def subset(from: Int, to: Int): WeightedDataset[T] = {
    new WeightedDataset[T](header, data.map(_.slice(from, to)), weights.slice(from, to))
  }

  def toDataset: Dataset[T] = new Dataset[T](header, data, isNumeric)

  def ++ (other: WeightedDataset[T]): WeightedDataset[T] = {
    WeightedDataset(this.toDataset ++ other.toDataset, weights ++ other.weights)
  }

}


object WeightedDataset {
  def apply[T >: DoubleCellT with IntCellT with CatCellT with Double](dataset: Dataset[T], weights: ParVector[Double]): WeightedDataset[T] = new WeightedDataset[T](dataset.header, dataset.data, weights, dataset.isNumeric)
  def apply[T >: DoubleCellT with IntCellT with CatCellT with Double](dataset: Dataset[T], weights: Vector[Double]): WeightedDataset[T] = apply(dataset, weights.par)
}
