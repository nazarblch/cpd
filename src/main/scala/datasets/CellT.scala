package datasets



trait CellT[T >: Int with Double with String] {
  def isNumeric: Boolean
  def data: T
  override def toString = data.toString
  def getType: String
  def toDouble: Double
}

object CellT {

  type CellType = CellT[_ >: Int with Double with String]

  val INT_TYPE_NAME = "int"
  val DOUBLE_TYPE_NAME = "double"
  val CAT_TYPE_NAME = "string"

  def apply(valueType: String, value: String): CellType = valueType match {
    case INT_TYPE_NAME => new IntCellT(value.toInt)
    case DOUBLE_TYPE_NAME => new DoubleCellT(value.toDouble)
    case CAT_TYPE_NAME => new CatCellT(value)
  }
}


class DoubleCellT(val data: Double) extends CellT[Double] {
  override def isNumeric: Boolean = true

  override def getType: String = CellT.DOUBLE_TYPE_NAME

  override def toDouble: Double = data
}


class IntCellT(val data: Int) extends CellT[Int] {
  override def isNumeric: Boolean = true

  override def getType: String = CellT.INT_TYPE_NAME

  override def toDouble: Double = data
}

class CatCellT(val data: String) extends CellT[String] {
  override def isNumeric: Boolean = false

  override def getType: String = CellT.CAT_TYPE_NAME

  override def toDouble: Double = data.toDouble
}


