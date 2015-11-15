//package models
//
//
//import breeze.linalg.{DenseMatrix, DenseVector}
//import datasets.{CatCellT, WeightedDataset, DataHeader, OneColumnDataset}
//import ru.ispras.modis.tm.attribute.DefaultAttributeType
//import ru.ispras.modis.tm.builder.FixedPhiBuilder
//import ru.ispras.modis.tm.documents.{TextualDocument, Numerator, Document, Alphabet}
//import ru.ispras.modis.tm.plsa.TrainedModelSerializer
//import ru.ispras.modis.tm.sparsifier.CarefulSparcifier
//
//
///**
// * Created by buzun on 21/08/15.
// */
//class PLSAModel extends ParametricModel[CatCellT, OneColumnDataset[CatCellT], DenseVector[Double]] {
//
//
//
//    val loaded = TrainedModelSerializer.load("/Users/buzun/tm/examples/model")
//    val alph =  Alphabet(loaded.alphabet)
//
//  def mkDoc(data: OneColumnDataset[CatCellT]): Document = {
//    Numerator(Iterator(new TextualDocument(Map(DefaultAttributeType -> data.getColumnsT.head.data.map(_.data)))), alph).head
//  }
//
//
//  override def likelihood(dataset: WeightedDataset[CatCellT, OneColumnDataset[CatCellT]], parameter: DenseVector[Double]): Double = {
//    val fixed = new FixedPhiBuilder(alph, Array(mkDoc(dataset.toDataset)), 5, loaded.phi, loaded.attributeWeight)
//      .setThetaSparsifier(new CarefulSparcifier(0.1f, 1, 3))
//      .build()
//
//    val res = fixed.logLikelihood(parameter.toArray)
//    // println(res)
//    res
//  }
//
//  override def fisherMatrix(dataset: WeightedDataset[CatCellT, OneColumnDataset[CatCellT]]): DenseMatrix[Double] = null
//
//  override def gradLikelihood(dataset: WeightedDataset[CatCellT, OneColumnDataset[CatCellT]], parameter: DenseVector[Double]): DenseVector[Double] = null
//
//  override def likelihood(dataset: WeightedDataset[CatCellT, OneColumnDataset[CatCellT]]): Double = {
//    val fixed = new FixedPhiBuilder(alph, Array(mkDoc(dataset.toDataset)), 5, loaded.phi, loaded.attributeWeight)
//      .setThetaSparsifier(new CarefulSparcifier(0.1f, 1, 3))
//      .build()
//
//    //println(fixed.predictTheta()._1)
//
//    fixed.predictTheta()._1
//  }
//
//  override def MLE(dataset: WeightedDataset[CatCellT, OneColumnDataset[CatCellT]]): DenseVector[Double] = {
//    val fixed = new FixedPhiBuilder(alph, Array(mkDoc(dataset.toDataset)), 5, loaded.phi, loaded.attributeWeight)
//      .setThetaSparsifier(new CarefulSparcifier(0.1f, 1, 3))
//      .build()
//
//    println(fixed.predictTheta()._1)
//
//    DenseVector(fixed.predictTheta()._2)
//  }
//
//  override def header: DataHeader = DataHeader(1)
//
//  override def dim: Int = 50
//}
