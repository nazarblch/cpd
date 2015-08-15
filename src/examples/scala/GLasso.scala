import breeze.linalg.{inv, DenseVector, DenseMatrix}
import breeze.stats.distributions.{MultivariateGaussian, Gaussian}
import datasets._
import likelihood_optimize.AdaptiveGradientDescentOptimizer
import models.glasso.GLassoModel
import breeze.stats._
import breeze.numerics._
import patterns.{PatternMatcher, MatcherResult, TrianglePattern}
import statistics.likelihood_ratio.LikelihoodRatioStatistic
import viz.utils.PlotXY

import scala.collection.parallel.immutable.ParVector


object GLasso extends App {

  val S1 = DenseMatrix.eye[Double](3)
  S1(1,2) = 0.3
  S1(2,1) = 0.3
  S1(1,0) = 0.2
  S1(0,1) = 0.2

  val S2 = DenseMatrix.eye[Double](3)
  S2(1,2) = 0.1
  S2(2,1) = 0.1
  S2(1,0) = 0.0
  S2(0,1) = 0.0
  S2(2,0) = 0.5
  S2(0,2) = 0.5



  val X1 = MultivariateGaussian(DenseVector.zeros[Double](3), S1).sample(300)
  val X2 = MultivariateGaussian(DenseVector.zeros[Double](3), S2).sample(300)

  def header: DataHeader = new DataHeader(IndexedSeq("double", "double", "double"))

  val col1: Column[Double] = Column[Double](X1.map(v => v(0)) ++ X2.map(v => v(0)))
  val col2: Column[Double] = Column[Double](X1.map(v => v(1)) ++ X2.map(v => v(1)))
  val col3: Column[Double] = Column[Double](X1.map(v => v(2)) ++ X2.map(v => v(2)))
  val data = new MultiColumnDataset[Double](header, Vector(col1, col2, col3), true)

  //LoadData.print(data)

  val model = new AdaptiveGradientDescentOptimizer[Vector[Double], MultiColumnDataset[Double]](DenseMatrix.eye[Double](3).toDenseVector, new GLassoModel(3, header))

  val wdata = WeightedDataset(data, ParVector.fill[Double](600)(1.0))

  println(model.likelihood(wdata))


  println(model.MLE(wdata))


//  val LRTs = new LikelihoodRatioStatistic[Double,Dataset[Double]](model, 150)
//  val res = LRTs.getValue(data)
//  println(res.mkString(", "))
//
//  val pl = new PlotXY("t", "LRTs")
//  pl.addline(res, "")
//  pl.print("test.png")




}
