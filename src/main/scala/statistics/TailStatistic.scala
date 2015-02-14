package statistics

import scala.util.Sorting

class TailStatistic(val sample: Array[Double]) {

  val EPS = 0.01

  val sorted: Array[Double] = sample.clone()
  Sorting.quickSort(sorted)

  def quantile(geqPr: Double): Double = {
    val leqPr: Double = 1.0 - geqPr
    val orderStatistic: Int = (sample.size * leqPr).toInt
    sorted(orderStatistic)
  }

  def getPValue(s: Double): Double = {
    var tailSize = 0

    var i = sorted.size - 1
    while (sorted(i) >= s && i >= 0) {
      tailSize += 1
      i -= 1
    }

    tailSize.toDouble / sorted.size
  }

  def getLogPValue(s: Double): Double = {
    val p = getPValue(s)
    if (p < EPS) math.log(EPS) else math.log(p)
  }

}
