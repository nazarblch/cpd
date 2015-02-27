package cp_detectors

import datasets.CellT._
import datasets.Dataset

import scala.collection.mutable.ArrayBuffer


class OnlineAdapter[T >: TCellDouble](val detector: OfflineChangePointDetector[T],
                                      val min_buf_size: Int,
                                      val offset: Int) extends OnlineChangePointDetector[T] {

  var buf: Option[Dataset[T]] = None

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

  override def init(dataset: Dataset[T]): Unit = {
    detector.init(dataset)
  }

  override def name: String = detector.name

  override def addData(row: IndexedSeq[T]): Unit = {
    if (buf.isEmpty) {
      buf = Some(Dataset(IndexedSeq(row)))
    } else {
      buf = Some(buf.get ++ row)
    }
  }
}
