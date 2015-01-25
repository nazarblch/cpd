import breeze.linalg.DenseMatrix
import datasets._
import likelihood_optimize.AdaptiveGradientDescentOptimizer
import models.glasso.GLassoModel
import statistics.likelihood_ratio.LikelihoodRatioStatistic
import viz.utils.PlotXY

import scala.collection.parallel.immutable.ParVector

object BrainModel extends App {

  val data: Dataset[Double] = DatasetConverter.toNumeric(DatasetLoader.loadFromFile("data/brain/setlsetc.csv"))

  LoadData.print(data)

  def header: DataHeader = new DataHeader(
    IndexedSeq("Subcallosal.Gyrus", "Transverse.Temporal.Gyrus","Rectal.Gyrus","Fusiform.Gyrus","Inferior.Occipital.Gyrus","Inferior.Temporal.Gyrus","Parahippocampal.Gyrus","Lingual.Gyrus","Middle.Occipital.Gyrus","Orbital.Gyrus","Middle.Temporal.Gyrus","Superior.Temporal.Gyrus","Superior.Occipital.Gyrus","Precentral.Gyrus","Inferior.Frontal.Gyrus","Angular.Gyrus","Supramarginal.Gyrus","Cingulate.Gyrus","Middle.Frontal.Gyrus","Postcentral.Gyrus","Superior.Frontal.Gyrus","Medial.Frontal.Gyrus"))

  //val model = new AdaptiveGradientDescentOptimizer[Double](DenseMatrix.eye[Double](header.size).toDenseVector, new GLassoModel(header.size, header))
  val model = new GLassoModel(header.size, header)


  val wdata = WeightedDataset(data, ParVector.fill[Double](data.size)(1.0))

  println(model.likelihood(wdata))
  println(model.MLE(wdata))

  val LRTs = new LikelihoodRatioStatistic[Double,Dataset[Double]](model, 600)
  val res = LRTs.getValueSync(data)
  println(res.mkString(", "))

  val pl = new PlotXY("t", "LRTs")
  pl.addline(res, "")
  pl.print("test.png")

}
