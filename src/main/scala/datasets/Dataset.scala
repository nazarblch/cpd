package datasets

import java.io.FileWriter

import breeze.linalg.DenseVector
import datasets.CellT.{CellType, TCellDouble}

import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.mutable.ParArray

trait ColumnT[T, C] {
  def size: Int
  def slice(from: Int, to: Int): C
  def apply(index: Int): T
  def ++ (other: C): C
  def :+ (elem: T): C
  def splitAt(index: Int): (C, C)
  def isNumeric: Boolean
  def subset(indexes: Array[Int]): C
}

class Column[T] (val data: Vector[T]) extends ColumnT[T, Column[T]] {
  override def size = data.length
  override def slice(from: Int, to: Int): Column[T] = new Column[T](data.slice(from, to))
  override def splitAt(index: Int): (Column[T], Column[T]) = {
    val (data1, data2) = data.splitAt(index)
    (new Column[T](data1), new Column[T](data2))
  }
  override def apply(index: Int): T = data(index)
  override def ++ (other: Column[T]): Column[T] = new Column[T](data ++ other.data)
  override def :+ (elem: T): Column[T] = new Column[T](data :+ elem)
  override def isNumeric: Boolean = data(0).isInstanceOf[Double] || data(0).asInstanceOf[CellType].isNumeric
  def getType: String = {
    if (data(0).isInstanceOf[Double]) CellT.DOUBLE_TYPE_NAME else data(0).asInstanceOf[CellType].getType
  }
  override def subset(indexes: Array[Int]): Column[T] = {
    new Column[T](indexes.toVector.map(i => data(i)))
  }
}

object Column {
  def apply[T](v: IndexedSeq[T]): Column[T] = {
     new Column[T](v.toVector)
  }

  def apply[T](): Column[T] = {
    new Column[T](Vector())
  }
}


abstract class Dataset[Row, Self](val header: DataHeader,
                                  val isNumeric: Boolean = false) {


  val rows: Vector[Row] = Vector.range(0, size).map(i => getRow(i))

  def size: Int

  def dim: Int = header.size

  def getRow(index: Int): Row

  def getRowsIterator: IndexedSeq[Row] = {
    rows
  }

  def toMatrix: Array[Vector[String]]

  def subset(from: Int, to: Int): Self

  def subset(indexes: Array[Int]): Self

  def splitAt(index: Int): (Self, Self)

  def ++ (other: Self): Self

  def :+ (row: Row): Self

  def getSelf: Self

  def save(path: String): Unit

  def convolution(f: (Row => Double)): Double

  def map(f: (Row => Double)): IndexedSeq[Double]

  def mapV(f: (Row => DenseVector[Double])): IndexedSeq[DenseVector[Double]]

  def getColumns: Vector[Column[Double]]

  def dropCol(num: Int): Self

}


class MultiColumnDataset[T >: TCellDouble](override val header: DataHeader,
                                           val data: Vector[Column[T]],
                                           override val isNumeric: Boolean = false,
                                           val classIndex: Option[Int] = None)
  extends Dataset[Vector[T], MultiColumnDataset[T]](header, isNumeric) {

  // assert(data(0).size > 0)
  assert(data.forall(_.size == data(0).size))
  assert(header.size == data.length)

  override def size = data(0).size

  override  def getRow(index: Int): Vector[T] = {
    data.map(col => col(index))
  }

  override  def subset(from: Int, to: Int): MultiColumnDataset[T] = {
    new MultiColumnDataset[T](header, data.map(_.slice(from, to)))
  }

  override  def splitAt(index: Int): (MultiColumnDataset[T], MultiColumnDataset[T]) = {
    val cols = data.map(_.splitAt(index))
    (new MultiColumnDataset[T](header, cols.map(_._1)), new MultiColumnDataset[T](header, cols.map(_._2)))
  }

  override  def ++ (other: MultiColumnDataset[T]): MultiColumnDataset[T] = {
    assert(header equals other.header)
    val newData: Vector[Column[T]] = data.zip(other.data).map({case (col1, col2) => col1 ++ col2})
    new MultiColumnDataset[T](header, newData, isNumeric)
  }

  override  def :+ (row: Vector[T]): MultiColumnDataset[T] = {
    assert(row.length equals dim)
    val newData: Vector[Column[T]] = data.zip(row).map({case (col1, elem) => col1 :+ elem})
    assert(newData.length == dim)
    new MultiColumnDataset[T](header, newData, isNumeric)
  }

  override  def save(path: String): Unit = {
    val head: String = header.data.zip(data.map(_.getType)).map{case(name, t) => name + ":" + t}.mkString(",")
    val fw = new FileWriter(path)
    fw.write(head + "\n")

    getRowsIterator.foreach(row => fw.write(row.mkString(",") + "\n"))

    fw.close()
  }

  override def getSelf: MultiColumnDataset[T] = this

  override def convolution(f: (Vector[T]) => Double): Double = {
    getRowsIterator.foldLeft(0.0)({case(res, row) => res + f(row)})
  }

  override def toMatrix: Array[Vector[String]] = {
    getRowsIterator.map(row => row.map(_.toString)).toArray
  }

  override def map(f: (Vector[T]) => Double): IndexedSeq[Double] = getRowsIterator.map(f)

  override def mapV(f: (Vector[T]) => DenseVector[Double]): IndexedSeq[DenseVector[Double]] = getRowsIterator.map(f)

  override def getColumns: Vector[Column[Double]] = data.map(_.asInstanceOf[Column[Double]])

  def concat(other: MultiColumnDataset[T]): MultiColumnDataset[T] = {
    val newHeader: DataHeader = header ++ other.header
    val newData: Vector[Column[T]] = data ++ other.data

    assert(size == other.size)
    assert(isNumeric == other.isNumeric)

    new MultiColumnDataset[T](newHeader, newData, isNumeric)
  }

  override def dropCol(num: Int): MultiColumnDataset[T] = {
    val newHeader: DataHeader = new DataHeader(header.data.slice(0, num) ++ header.data.slice(num+1, dim), header.types.slice(0, num) ++ header.types.slice(num+1, dim))
    val newData: Vector[Column[T]] = data.slice(0, num) ++ data.slice(num+1, dim)
    new MultiColumnDataset[T](newHeader, newData, isNumeric)
  }

  override def subset(indexes: Array[Int]): MultiColumnDataset[T] = new MultiColumnDataset[T](header, data.map(_.subset(indexes)), isNumeric, classIndex)
}


class OneColumnDataset[T >: TCellDouble](override val header: DataHeader,
                                         val data: Column[T],
                                         override val isNumeric: Boolean = false)
  extends Dataset[T, OneColumnDataset[T]](header, isNumeric) {


  assert(header.size == 1)

  override def size = data.size

  override def getRow(index: Int): T = {
    data(index)
  }

  def subset(from: Int, to: Int): OneColumnDataset[T] = {
    new OneColumnDataset[T](header, data.slice(from, to))
  }

  def splitAt(index: Int): (OneColumnDataset[T], OneColumnDataset[T]) = {
    val cols = data.splitAt(index)
    (new OneColumnDataset[T](header, cols._1), new OneColumnDataset[T](header, cols._2))
  }

  def ++ (other: OneColumnDataset[T]): OneColumnDataset[T] = {
    assert(header equals other.header)
    new OneColumnDataset[T](header, data ++ other.data, isNumeric)
  }

  def :+ (row: T): OneColumnDataset[T] = {
    new OneColumnDataset[T](header, data :+ row, isNumeric)
  }

  override def getRowsIterator: IndexedSeq[T] = {
    data.data.toVector
  }

  def save(path: String): Unit = {
    val head: String = header.data(0) + ":" + data.getType
    val fw = new FileWriter(path)
    fw.write(head + "\n")

    getRowsIterator.foreach(row => fw.write(row + "\n"))

    fw.close()
  }

  override def getSelf: OneColumnDataset[T] = this

  override def convolution(f: (T) => Double): Double = {
    data.data.foldLeft(0.0)({case(res, row) => res + f(row)})
  }

  override def toMatrix: Array[Vector[String]] = {
    getRowsIterator.map(row => Vector(row.toString)).toArray
  }

  override def map(f: (T) => Double): Vector[Double] = data.data.map(f)

  override def mapV(f: (T) => DenseVector[Double]): Vector[DenseVector[Double]] = data.data.map(f)

  override def getColumns: Vector[Column[Double]] = Vector(data.asInstanceOf[Column[Double]])

  def getColumnsT: Vector[Column[T]] = Vector(data.asInstanceOf[Column[T]])

  override def dropCol(num: Int): OneColumnDataset[T] = this

  override def subset(indexes: Array[Int]): OneColumnDataset[T] = new OneColumnDataset[T](header, data.subset(indexes), isNumeric)
}


object Dataset {


  def apply[T >: TCellDouble](rows: IndexedSeq[IndexedSeq[T]]): MultiColumnDataset[T] = {
    val header = DataHeader(rows(0).length)
    val cols: Vector[Column[T]] =
      Vector.range(0, header.size).map(j => Column(rows.map(row => row(j)).toVector))
    new MultiColumnDataset[T](header, cols, true)
  }

  def apply[T >: TCellDouble](rows: IndexedSeq[T]): OneColumnDataset[T] = {
    val header = DataHeader(1)
    new OneColumnDataset[T](header, Column(rows.toVector), true)
  }


  def applyVec(data: IndexedSeq[DenseVector[Double]]): DenseVectorDataset = {
    val header = DataHeader(data(0).length)
    new DenseVectorDataset(header, data.toArray)
  }

  implicit def applyD(data: IndexedSeq[Double]): MultiColumnDataset[Double] = {
    val header = DataHeader(1)
    new MultiColumnDataset[Double](header, Vector(Column[Double](data.toVector)), true)
  }

  implicit def applyB(data: IndexedSeq[Boolean]): MultiColumnDataset[Double] = {
    applyD(data.map(x => if (x) 1.0 else 0.0))
  }

}


object DatasetConverter {
  def toDoubleColumn(column: Column[CellType]): Column[Double] = {
    assert(column.isNumeric)
    new Column[Double](column.data.map(_.toDouble))
  }

  def toNumeric(dataset: MultiColumnDataset[CellType]): DenseVectorDataset = {
     new DenseVectorDataset(dataset.header, dataset.getRowsIterator.toArray.map(r => DenseVector(r.toArray.map(_.toDouble))))
  }

  def toNumeric(dataset: OneColumnDataset[CellType]): OneColumnDataset[Double] = {
    new OneColumnDataset[Double](dataset.header, toDoubleColumn(dataset.data), true)
  }
}


class WeightedDataset[Row, Self <: Dataset[Row, Self]](
                                                           dataset: Self,
                                                           val weights: Vector[Double]
                                  ) extends Dataset[Row, WeightedDataset[Row, Self]](dataset.header, dataset.isNumeric) {


  def getRowWithWeight(index: Int): (Row, Double) = {
    (dataset.getRow(index), weights(index))
  }

  def getRowsWithWeightIterator: IndexedSeq[(Row, Double)] = dataset.getRowsIterator.zip(weights)

  override def convolution(f: (Row => Double)): Double = {
    dataset.map(f).zip(weights).map{case (r,w) => r * w}.sum
  }

  def convolutionV(f: (Row => DenseVector[Double])): DenseVector[Double] = {
    dataset.mapV(f).zip(weights).map{case (r,w) => r * w}.reduce(_+_)
  }

  override def subset(from: Int, to: Int): WeightedDataset[Row, Self] = {
    new WeightedDataset[Row, Self](dataset.subset(from, to), weights.slice(from, to))
  }

  def toDataset: Self = dataset

  override def ++ (other: WeightedDataset[Row, Self]): WeightedDataset[Row, Self] = {
    new WeightedDataset(dataset ++ other.toDataset.getSelf, weights ++ other.weights)
  }

  override def size: Int = dataset.size

  override def getSelf: WeightedDataset[Row, Self] = this

  override def splitAt(index: Int): (WeightedDataset[Row, Self], WeightedDataset[Row, Self]) = {
    val data_sp = dataset.splitAt(index)
    val w_sp = weights.splitAt(index)
    (new WeightedDataset[Row, Self](data_sp._1, w_sp._1),
     new WeightedDataset[Row, Self](data_sp._2, w_sp._2))
  }

  override def getRow(index: Int): Row = dataset.getRow(index)

  override def save(path: String): Unit = dataset.save(path)

  override def :+(row: Row): WeightedDataset[Row, Self] = null

  override def toMatrix: Array[Vector[String]] = null

  override def map(f: (Row) => Double): Vector[Double] = null

  override def mapV(f: (Row) => DenseVector[Double]): Vector[DenseVector[Double]] = null

  override def getColumns: Vector[Column[Double]] = null

  override def dropCol(num: Int): WeightedDataset[Row, Self] = new WeightedDataset[Row, Self](dataset.dropCol(num), weights)

  override def subset(indexes: Array[Int]): WeightedDataset[Row, Self] = null
}


object WeightedDataset {
  def apply[Row, Self <: Dataset[Row, Self]](dataset: Dataset[Row, Self], weights: ParVector[Double]): WeightedDataset[Row, Self] = new WeightedDataset[Row, Self](dataset.getSelf, weights.toVector)
  def apply[Row, Self <: Dataset[Row, Self]](dataset: Dataset[Row, Self], weights: Vector[Double]): WeightedDataset[Row, Self] = new WeightedDataset[Row, Self](dataset.getSelf, weights)
}
