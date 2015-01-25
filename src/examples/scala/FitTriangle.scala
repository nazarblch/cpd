import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import patterns.{MatcherResult, PatternMatcher, TrianglePattern}
import viz.utils.PlotXY

object FitTriangle extends App {

  def genConst(length: Int, value: Double): DenseVector[Double] = DenseVector.fill[Double](length)(value)

  def genLine(length: Int, v1: Double, v2: Double): DenseVector[Double] = DenseVector.range(0, length).map(i => {
    v1 + i * (v2 - v1) / length.toDouble
  })

  def genTriangle(length: Int, v1: Double, v2: Double): DenseVector[Double] = {
    DenseVector[Double](
      genLine(length/2, v1, v2).data ++ genLine(length - length/2, v2, v1).data
    )
  }

  def addNoise(data: DenseVector[Double], sigma: Double = 1) = {
    val r = new Gaussian(0, sigma)
    data.map(_ + r.draw())
  }

  val ys1 = genConst(20, 3.0)
  val ys2 = genTriangle(40, 3.0, 10.0)
  val ys3 = genConst(20, 3.0)
  val ys = addNoise(DenseVector[Double](ys1.data ++ ys2.data ++ ys3.data), 2.0)

  val pattern = new TrianglePattern(40)

  val res: MatcherResult = PatternMatcher.exec(pattern, ys)

  println(res.offset)
  //println(res.pattern.getPlot(res.offset, 80))

  val pl = new PlotXY("x", "y")

  pl.addline(ys,"")
  pl.addline(res.pattern.getPlot(res.offset, 80), "")

  pl.print("test.png")

}
