package datasets

import java.io.FileWriter

import breeze.linalg.DenseVector
import datasets.CellT.TCellDouble

class DenseVectorDataset(override val header: DataHeader,
                                           val data: Array[DenseVector[Double]],
                                           val classIndex: Option[Int] = None)
  extends Dataset[DenseVector[Double], DenseVectorDataset](header, true) {

  // assert(data(0).size > 0)
  assert(data.forall(_.size == data(0).size))
  assert(header.size == data.head.length)

  override def size = data.length

  override  def getRow(index: Int): DenseVector[Double] = {
    data(index)
  }

  override  def subset(from: Int, to: Int): DenseVectorDataset = {
    new DenseVectorDataset(header, data.slice(from, to))
  }

  override  def splitAt(index: Int): (DenseVectorDataset, DenseVectorDataset) = {
    val split = data.splitAt(index)
    (new DenseVectorDataset(header, split._1, classIndex), new DenseVectorDataset(header, split._2, classIndex))
  }

  override  def ++ (other: DenseVectorDataset): DenseVectorDataset = {
    assert(header equals other.header)
    new DenseVectorDataset(header, data ++ other.data, classIndex)
  }

  override  def :+ (row: DenseVector[Double]): DenseVectorDataset  = {
    assert(row.length equals dim)
    new DenseVectorDataset(header, data ++ Array(row), classIndex)
  }

  override  def save(path: String): Unit = {

  }

  override def getSelf: DenseVectorDataset = this

  override def convolution(f: (DenseVector[Double]) => Double): Double = {
    getRowsIterator.foldLeft(0.0)({case(res, row) => res + f(row)})
  }

  override def toMatrix: Array[Vector[String]] = {
    getRowsIterator.map(row => row.map(_.toString).toArray.toVector).toArray
  }

  override def map(f: (DenseVector[Double]) => Double): IndexedSeq[Double] = getRowsIterator.map(f)

  override def mapV(f: (DenseVector[Double]) => DenseVector[Double]): IndexedSeq[DenseVector[Double]] = getRowsIterator.map(f)

  override def getColumns: Vector[Column[Double]] = null

  override def dropCol(num: Int): DenseVectorDataset = {
    val newHeader: DataHeader = new DataHeader(header.data.slice(0, num) ++ header.data.slice(num+1, dim), header.types.slice(0, num) ++ header.types.slice(num+1, dim))
    new DenseVectorDataset(newHeader, data.map(r => DenseVector(r.slice(0, num).toArray ++ r.slice(num + 1, dim).toArray)), classIndex)
  }

  override def subset(indexes: Array[Int]): DenseVectorDataset = new DenseVectorDataset(header, indexes.map(i => data(i)), classIndex)
}