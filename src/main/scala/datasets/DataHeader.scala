package datasets

class DataHeader(val data: IndexedSeq[String]) {
  def equals(other: DataHeader): Boolean = {
    if (other.data.length != data.length) {
      false
    } else {
      other.data.zip(data).forall({case (h1, h2) => h1 equals h2})
    }
  }

  def size = data.size
}
