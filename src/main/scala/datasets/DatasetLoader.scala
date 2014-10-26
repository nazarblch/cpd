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

  def loadFromFile(path: String): Dataset[CellType] = {

    assert(path.split("\\.").last equals "csv")

    // read header
    val lineIterator = io.Source.fromFile(path).getLines()
    val topLine = lineIterator.next()
    val header: Array[String] = topLine.trim.split(",").map(_.trim.split(":")(0))
    val types: Array[String] = topLine.trim.split(",").map(_.trim.split(":")(1))

    // read data
    val data: Array[ArrayBuffer[String]] = Array.fill(header.size)(ArrayBuffer[String]())

    while (lineIterator.hasNext) {
      val line: String = lineIterator.next()
      val strrow = line.trim.split(",")
      assert(strrow.length == header.length)

      for(i <- 0 until header.size) {
        data(i).append(strrow(i))
      }
    }

    val dataBuilder = ParVector.newBuilder[Column[CellType]]
    Range(0, header.size).foreach(i => {
      val vb = ParArray.newBuilder[CellType]
      val t: String = types(i)
      data(i).foreach(elem => vb += CellT(t, elem))
      dataBuilder += new Column[CellType](vb.result())
    })

    new Dataset[CellType](
      new DataHeader(header.toIndexedSeq),
      dataBuilder.result()
    )

  }

}



