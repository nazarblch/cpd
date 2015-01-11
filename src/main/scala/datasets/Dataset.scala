package datasets

import breeze.linalg.DenseVector
import datasets.CellT.CellType

import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.mutable.ParArray

class Column[T] (val data: ParArray[T]) {
  def size = data.length
  def slice(from: Int, to: Int): Column[T] = new Column[T](data.slice(from, to))
  def apply(index: Int): T = data(index)
  def isNumeric: Boolean = data(0).isInstanceOf[Double] || data(0).asInstanceOf[CellType].isNumeric
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

  def getRow(index: Int): IndexedSeq[T] = {
    for (col <- 0 until header.size) yield data(col)(index)
  }

  def getRowsIterator: IndexedSeq[IndexedSeq[T]] = {
    for (i <- 0 until size) yield getRow(i)
  }

  def subset(from: Int, to: Int): Dataset[T] = {
    new Dataset[T](header, data.map(_.slice(from, to)))
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



}
