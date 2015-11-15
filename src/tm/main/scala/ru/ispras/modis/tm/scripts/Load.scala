package ru.ispras.modis.tm.scripts

import java.io.File

import breeze.linalg.DenseVector
import ru.ispras.modis.tm.attribute.DefaultAttributeType
import ru.ispras.modis.tm.builder.FixedPhiBuilder
import ru.ispras.modis.tm.documents.{Alphabet, TextualDocument, Document, Numerator}
import ru.ispras.modis.tm.plsa.TrainedModelSerializer
import ru.ispras.modis.tm.sparsifier.CarefulSparcifier
import ru.ispras.modis.tm.utils.PlotXY

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Created by buzun on 16/08/15.
 */
object Load extends App {

  val loaded = TrainedModelSerializer.load("examples/model")

  def getWindDocuments(h: Int): Iterator[TextualDocument] = {
    /**
     * read lines from textual file with a 1000 scientific articles
     */
    val lines = Source.fromFile(new File("/Users/buzun/gb/data/docs.txt")).getLines()

    /**
     * split each line by space
     */
    val wordsSequence: Vector[String] = lines.flatMap(line => line.split(" ")).toVector
    val whc = wordsSequence.length / h

    val ranges = Vector.range(0, whc - 1).flatMap(i => Array((i * h + 0, i * h + h), (i * h + h, i * h + 2* h), (i * h + 0, i * h + 2* h)))

    /**
     * now we obtain a sequences of sequence of words and should to construct textual documents.
     * Textual document may contain a few texts, corresponding to different  attributes,
     * for example text in english, translation of this text to russian etc. If you document contain only one text
     * you may use attribute Category
     */
    val textualDocuments = ranges.map(rr => new TextualDocument(Map(DefaultAttributeType -> wordsSequence.slice(rr._1, rr._2))))

    /**
     * and now we return sequence of textual documents
     */
    textualDocuments.toIterator
  }

  val alph =  Alphabet(loaded.alphabet)
  val documents = Numerator(getWindDocuments(1000), alph)

  /**
   * and now we may load model
   */


  val buf: ArrayBuffer[Double] = ArrayBuffer()
  val buf_th: ArrayBuffer[Array[Double]] = ArrayBuffer()



  for (d <- documents.slice(0, 3000)) {

    val fixed = new FixedPhiBuilder(alph, Array(d), 1, loaded.phi, loaded.attributeWeight)
      .setThetaSparsifier(new CarefulSparcifier(0.1f, 1, 3))
      .build()

    val (ll, theta) = fixed.predictTheta()

    buf += ll
    buf_th += theta

  }



  val pl = new PlotXY("x", "y")



  pl.addline(
    DenseVector(
      buf.result().toVector.grouped(3).map(b => b(0) + b(1) - b(2)).toArray
    ),
    "ll")

//  buf_th.result().toVector.grouped(3).foreach(b => println( b(0).zip(b(1)).map({case (d1, d2) => math.abs(d1 - d2).toFloat}).mkString(",")) )
//
//  pl.addline(
//    DenseVector(
//      buf_th.result().toVector.grouped(3).map(b => b(0).zip(b(1)).map({case (d1, d2) => math.abs(d1 - d2)}).sum ).toArray
//    ),
//    "param")

  pl.print("test.png")

}
