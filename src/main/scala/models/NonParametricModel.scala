package models

import datasets.CellT.CellType
import datasets._
import statistics.EmpiricalDistributionFunction
import utils.Stat

/**
 * Non parametric model description could be found in parer
 * NONPARAMETRIC MAXIMUM LIKELIHOOD APPROACH TO MULTIPLE CHANGE-POINT PROBLEM
 * BY CHANGLIANG ZOU et al.
 * eq. (2.3)
 *
 */


class NonParametricModel(val header: DataHeader,
                         val dataset: Dataset[CellType]
                         ) extends Model[CellType] {

  // for 1-dim data only
  assert(dataset.data.length == 1)

  def sort(column: Column[CellType]): Column[CellType] = column(0).getType match {
    case CellT.DOUBLE_TYPE_NAME => Column[CellType](column.asInstanceOf[Column[DoubleCellT]].data.toVector.sortBy(_.data))
    case CellT.INT_TYPE_NAME => Column[CellType](column.asInstanceOf[Column[IntCellT]].data.toVector.sortBy(_.data))
    case CellT.CAT_TYPE_NAME => Column[CellType](column.asInstanceOf[Column[CatCellT]].data.toVector.sortBy(_.data))
  }

  val sorted_col: Column[CellType] = sort(dataset.data(0))

  override def likelihood(subset: WeightedDataset[CellType]): Double = {

    assert(subset.getRow(0)(0).getType equals sorted_col(0).getType)

    val n: Double = sorted_col.size
    val s: Double = subset.size
    val F = EmpiricalDistributionFunction

    sorted_col.data.zipWithIndex.map{case(cell, i) =>
      val Fi: Double = F.value(subset.data(0), cell)
      val Hi = Stat.H(Fi)
      if (i > 0 && i < n) (n * s / (i * (n - i))) * Hi else 0.0
    }.sum

  }

}
