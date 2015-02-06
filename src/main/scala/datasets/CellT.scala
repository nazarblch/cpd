package datasets

import datasets.CellT._


trait CellT[T >: Int with Double with String, C] {
  def isNumeric: Boolean
  def data: T
  override def toString = data.toString
  def getType: String
  def toDouble: Double
  def < (o: C): Boolean
  def >= (o: C): Boolean = ! < (o)
  def > (o: C): Boolean = ! >= (o)
  def <= (o: C): Boolean = ! > (o)

  def safeCast(o: CellType): C = {
    assert(o.isInstanceOf[C])
    cast(o)
  }
  def cast(o: CellType): C

}

object CellT {

  type CellType = CellT[_ >: Int with Double with String, _ >: DoubleCellT with IntCellT with CatCellT with Double]
  type TCellDouble = DoubleCellT with IntCellT with CatCellT with Double

  val INT_TYPE_NAME = "int"
  val DOUBLE_TYPE_NAME = "double"
  val CAT_TYPE_NAME = "string"

  def apply(valueType: String, value: String): CellType = valueType match {
    case INT_TYPE_NAME => new IntCellT(value.toInt)
    case DOUBLE_TYPE_NAME => new DoubleCellT(value.toDouble)
    case CAT_TYPE_NAME => new CatCellT(value)
  }

}


class DoubleCellT(val data: Double) extends CellT[Double, DoubleCellT] {
  override def isNumeric: Boolean = true

  override def getType: String = CellT.DOUBLE_TYPE_NAME

  override def toDouble: Double = data

  override def <(o: DoubleCellT): Boolean = data < o.data

  override def cast(o: CellType): DoubleCellT = o.asInstanceOf[DoubleCellT]
}


class IntCellT(val data: Int) extends CellT[Int, IntCellT] {
  override def isNumeric: Boolean = true

  override def getType: String = CellT.INT_TYPE_NAME

  override def toDouble: Double = data

  override def <(o: IntCellT): Boolean = data < o.data

  override def cast(o: CellType): IntCellT = o.asInstanceOf[IntCellT]
}

class CatCellT(val data: String) extends CellT[String, CatCellT] {
  override def isNumeric: Boolean = false

  override def getType: String = CellT.CAT_TYPE_NAME

  override def toDouble: Double = data.toDouble

  override def <(o: CatCellT): Boolean = data < o.data

  override def cast(o: CellType): CatCellT = o.asInstanceOf[CatCellT]
}


