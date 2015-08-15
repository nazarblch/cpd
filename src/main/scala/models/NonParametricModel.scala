package models

import datasets.CellT._
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
                         val dataset: OneColumnDataset[Double]
                         ) extends Model[Double, OneColumnDataset[Double]] {

  // for 1-dim data only
  assert(dataset.size == 1)

  def sort(column: Column[Double]): Column[Double] = {
    Column(column.data.sorted)
  }

  val sorted_col: Column[Double] = {
    sort(dataset.getColumns.head.asInstanceOf[Column[Double]])
  }

  override def likelihood(subset: WeightedDataset[Double, OneColumnDataset[Double]]): Double = {

    val n: Double = sorted_col.size
    val s: Double = subset.size
    val F = EmpiricalDistributionFunction

    sorted_col.data.zipWithIndex.map{case(cell, i) =>
      val Fi: Double = F.value(subset.getColumns.head, cell)
      val Hi = Stat.H(Fi)
      if (i > 0 && i < n) (n * s / (i * (n - i))) * Hi else 0.0
    }.sum

  }

}
