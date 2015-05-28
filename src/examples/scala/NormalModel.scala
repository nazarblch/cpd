import breeze.linalg.DenseVector
import breeze.stats.distributions.Gaussian
import datasets.{WeightedDataset, Column, DataHeader, Dataset}
import models.standart.NormalModel

import scala.collection.parallel.immutable.ParVector

/**
 * Created by nazar on 1/18/15.
 */
object NormalModel extends App {

//  def header: DataHeader = new DataHeader(IndexedSeq("double"))
//
//  val col: Column[Double] = new Column[Double](Gaussian(2,1).sample(100).toArray.par)
//  val data = new Dataset[Double](header, ParVector(col), true)
//  val wdata = WeightedDataset(data, Gaussian(1,1).sample(100).toVector)
//
//  LoadData.print(data)
//
//  val model = new NormalModel
//
//  println( model.likelihood(wdata, DenseVector(2.0) ))
//
//  println( model.MLE(wdata) )

}
