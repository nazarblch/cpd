package cp_detectors

import datasets.CellT._
import datasets.{OneColumnDataset, Column, DataHeader, Dataset}

import scala.collection.mutable.ArrayBuffer


class OnlineAdapter[Row, Self <: Dataset[Row, Self]](val detector: OfflineChangePointDetector[Row, Self],
                                      val min_buf_size: Int,
                                      val offset: Int) extends OnlineChangePointDetector[Row, Self] {

  var buf: Option[Self] = None

  override def hasNewChangePoint: Boolean = {

    if (buf.get.size < min_buf_size) return false

    val cp = detector.findAll(buf.get)
    if (cp.length > 0) {
      assert(cp.sorted.last < buf.get.size)
      val pos = math.min(cp.sorted.last + offset, buf.get.size)
      buf = Some(buf.get.subset(pos, buf.get.size))
      // println("size =" + buf.get.size)
    }
    cp.length > 0
  }

  override def clear(): Unit = {
    buf = None
  }

  override def init(dataset: Self): Unit = {
    detector.init(dataset)
  }

  override def name: String = detector.name

  override def addData(row: Row): Unit = {
    if (buf.isEmpty) {
      buf = Some(new OneColumnDataset[Double](DataHeader(1), Column.apply[Double](), true).asInstanceOf[Self] :+ row)
    } else {
      buf = Some(buf.get :+ row)
    }
  }
}
