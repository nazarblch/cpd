package datasets

class DataHeader(val data: IndexedSeq[String], val types: IndexedSeq[String]) {
  def equals(other: DataHeader): Boolean = {
    if (other.data.length != data.length) {
      false
    } else {
      other.data.zip(data).forall({ case (h1, h2) => h1 equals h2})
    }
  }

  def size = data.size

  def ++ (other: DataHeader): DataHeader = new DataHeader(data ++ other.data, types ++ other.types)
}

object DataHeader {
  def apply(size: Int): DataHeader = {
    new DataHeader(Vector.range(0, size).map("F_" + _), Vector.fill[String](size)("double"))
  }
}
