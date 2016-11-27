package datasets

import datasets.CellT.CellType

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.mutable.ParArray

/**
 *
 * data format
 *
 * file name: *.csv
 *
 * colname1:type1,colname2:type2,colname3:type3, ...
 * data11,data12,data13,...
 * data21,data22,data23,...
 * ...
 *
 * type in {int, double, string}
 */

object DatasetLoader {

  def loadHeader(path: String, from: Int, toOpt: Int, separator: String): DataHeader = {
    val lineIterator = io.Source.fromFile(path).getLines()
    val topLine = lineIterator.next().trim.split(separator)
    val to = if(toOpt > from) toOpt else topLine.size

    val header: Array[String] = topLine.map(_.trim.split(":")(0)).slice(from, to)
    val types: Array[String] = topLine.map(_.trim.split(":")(1)).slice(from, to)

    new DataHeader(header.toIndexedSeq, types)
  }

  def loadFromFileData(path: String, from: Int, toOpt: Int, separator: String, header: DataHeader): Vector[Column[CellType]] = {

    assert(path.split("\\.").last equals "csv")

    val lineIterator = io.Source.fromFile(path).getLines()
    val to = header.data.size

    val data: Array[ArrayBuffer[String]] = Array.fill(header.size)(ArrayBuffer[String]())

    while (lineIterator.hasNext) {
      val line: String = lineIterator.next()
      val strrow = line.trim.split(separator).slice(from, to)
      assert(strrow.length == header.data.length)

      for(i <- header.data.indices) {
        data(i).append(strrow(i))
      }
    }


    val dataBuilder = Vector.newBuilder[Column[CellType]]
    Range(0, header.size).foreach(i => {
      val vb = Vector.newBuilder[CellType]
      val t: String = header.types(i)
      data(i).foreach(elem => vb += CellT(t, elem))
      dataBuilder += new Column[CellType](vb.result())
    })

    dataBuilder.result()

  }

  def loadFromFile(path: String): OneColumnDataset[CellType] = {
    val header = loadHeader(path, 0, 1, ",")
    val data = loadFromFileData(path, 0, 1, ",", header)
    assert(data.size == 1)
    new OneColumnDataset[CellType](header, data.head)
  }

  def loadFromFileMulticol(path: String, from: Int = 0, to: Int = -1, separator: String = ","): MultiColumnDataset[CellType] = {
    val header = loadHeader(path, from, to, separator)
    val data = loadFromFileData(path, from, to, separator, header)
    assert(data.size > 1)
    new MultiColumnDataset[CellType](header, data)
  }

  def loadFromFileMulticolDouble(path: String, from: Int = 0, to: Int = -1, separator: String = ","): DenseVectorDataset = {
    val header = DataHeader(io.Source.fromFile(path).getLines().next().split(separator).length)
    val data = loadFromFileData(path, from, to, separator, header)
    assert(data.size > 1)
    DatasetConverter.toNumeric(new MultiColumnDataset[CellType](header, data))
  }

}



