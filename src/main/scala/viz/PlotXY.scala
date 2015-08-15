package viz

package utils


import java.io.FileWriter

import breeze.plot._
import breeze.linalg._

import scala.collection.mutable.ArrayBuffer

class PlotXY(xlabel: String, ylabel: String) {

  val f = Figure()
  val p = f.subplot(0)
  p.xlabel = xlabel
  p.ylabel = ylabel
  p.legend = true
  p.setYAxisDecimalTickUnits()

  p.setXAxisDecimalTickUnits()
  // p.logScaleX = true

  val lines = ArrayBuffer[(String, DenseVector[Double], DenseVector[Double])]()


  var lc = 0

  def addline(x: DenseVector[Double], y: DenseVector[Double], name: String) {
    p += plot(x, y, name = name, shapes = true)
    lc += 1
    if (lc > 1) p.legend = true

    lines += Triple(name, x, y)
  }

  def addline(y: DenseVector[Double], name: String) {
    val x:  DenseVector[Double] = DenseVector(Range(0, y.length).map(_.toDouble).toArray)
    p += plot(x, y, name = name, shapes = true)
    lines += Triple(name, x, y)
  }

  def addline(y: Array[Double], name: String) {
    addline(DenseVector[Double](y), name)
  }

  def addline(x: Array[Double], y: Array[Double], name: String) {
    addline(DenseVector[Double](x), DenseVector[Double](y), name)
  }

  def addline(y: Array[(Int, Double)], name: String) {
    addline(DenseVector(y.map(_._1.toDouble)), DenseVector[Double](y.map(_._2)), name)
  }

  def print(path: String) {
    f.saveas(path)
    val fw = new FileWriter(path + ".txt")

    lines.result().foreach({case(s, x, y) =>
      fw.write(s + "\n")
      x.toArray.zip(y.toArray).foreach({case(i,j) => fw.write(i + "," + j + "\n")})
    })

    fw.close()

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
