package datasets

import datasets.CellT.CellType

import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.mutable.ParArray

class Column[T >: CellType with Double] (val data: ParArray[T]) {
  def size = data.length
  def slice(from: Int, to: Int): Column[T] = new Column[T](data.slice(from, to))
  def apply(index: Int): T = data(index)
  def isNumeric: Boolean = data(0).isInstanceOf[Double] || data(0).asInstanceOf[CellType].isNumeric
}

class CellTColumn(override val data: ParArray[CellType]) extends Column[CellType](data) {
  def toDoubleColumn: Column[Double] = {
    assert(isNumeric)
    new Column[Double](data.map(_.toDouble))
  }
}

class Dataset[T >: CellType with Double](val header: IndexedSeq[String],
                 private val data: ParVector[Column[T]]
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

  def isNumeric: Boolean = false

}

class CellTDataset(override val header: IndexedSeq[String], private val data: ParVector[CellTColumn])
  extends Dataset[CellType](header, data) {

  def toNumeric: NumericDataset = new NumericDataset(header, data.map(_.toDoubleColumn))
}

class NumericDataset(override val header: IndexedSeq[String], private val data: ParVector[Column[Double]])
    extends Dataset[Double](header, data) {

  override def isNumeric: Boolean = true


}
