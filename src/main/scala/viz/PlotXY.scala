package viz

package utils

import breeze.plot._
import breeze.linalg._

class PlotXY(xlabel: String, ylabel: String) {

  val f = Figure()
  val p = f.subplot(0)
  p.xlabel = xlabel
  p.ylabel = ylabel
  p.legend = true


  var lc = 0

  def addline(x: DenseVector[Double], y: DenseVector[Double], name: String) {
    p += plot(x, y, name = name, shapes = true)
    lc += 1
    if (lc > 1) p.legend = true
  }

  def addline(y: DenseVector[Double], name: String) {
    val x:  DenseVector[Double] = DenseVector(Range(0, y.length).map(_.toDouble).toArray)
    p += plot(x, y, name = name, shapes = true)
  }

  def addline(y: Array[Double], name: String) {
    addline(DenseVector[Double](y), name)
  }

  def print(path: String) {
    f.saveas(path)
  }
}

object PlotApp extends App {
  val pl = new PlotXY("x", "y")



  pl.addline(
    DenseVector(
      0.91, 0.9, 0.85, 0.79, 0.68,
      0.91, 0.9, 0.85, 0.79, 0.68,
      0.91, 0.9, 0.85, 0.79, 0.68,
      0.91, 0.9, 0.85, 0.79, 0.68
    ),
    "")

  pl.print("test.png")
}
