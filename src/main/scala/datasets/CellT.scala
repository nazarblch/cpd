package datasets

import datasets.CellT._


trait CellT[T >: Int with Double with String] {
  def isNumeric: Boolean
  def data: T
  override def toString = data.toString
  def getType: String
  def toDouble: Double
  def < (o: CellT[T]): Boolean
  def >= (o: CellT[T]): Boolean = ! < (o)
  def > (o: CellT[T]): Boolean = ! >= (o)
  def <= (o: CellT[T]): Boolean = ! > (o)

  def safeCast(o: CellType): CellT[T] = {
    assert(o.isInstanceOf[CellT[T]])
    cast(o)
  }
  def cast(o: CellType): CellT[T]
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

  override def <(o: CellT[Double]): Boolean = data < o.data

  override def cast(o: CellType): DoubleCellT = o.asInstanceOf[DoubleCellT]
}


class IntCellT(val data: Int) extends CellT[Int] {
  override def isNumeric: Boolean = true

  override def getType: String = CellT.INT_TYPE_NAME

  override def toDouble: Double = data

  override def <(o: CellT[Int]): Boolean = data < o.data

  override def cast(o: CellType): CellT[Int] = o.asInstanceOf[IntCellT]
}

class CatCellT(val data: String) extends CellT[String] {
  override def isNumeric: Boolean = false

  override def getType: String = CellT.CAT_TYPE_NAME

  override def toDouble: Double = data.toDouble

  override def <(o: CellT[String]): Boolean = data < o.data

  override def cast(o: CellType): CellT[String] = o.asInstanceOf[CatCellT]
}


