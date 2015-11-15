import java.io.File

import datasets._
import generate.patterns.PatternGenerator
import models.PLSAModel

import models.standart.NormalModel
import patterns.TrianglePattern

import statistics.PatternStatistic
import statistics.likelihood_ratio.LikelihoodRatioStatistic
import viz.utils.PlotXY

import scala.io.Source

/**
 * Created by buzun on 21/08/15.
 */
object PLSA extends App {

  val model = new PLSAModel

  def getWindDocuments = {
    /**
     * read lines from textual file with a 1000 scientific articles
     */
    val lines = Source.fromFile(new File("/Users/buzun/gb/data/docs.txt")).getLines().toSeq.drop(20).filter(_.split(" ").size > 600)

    var ss = 0
    lines.foreach(l =>{
      ss += l.split(" ").size
      println(ss)
    } )


    /**
     * split each line by space
     */
    val wordsSequence: Vector[CatCellT] = lines.flatMap(line => line.split(" ")).toVector.map(s => new CatCellT(s)).slice(5002512, 5025255)
    new OneColumnDataset[CatCellT](DataHeader(1), new Column(wordsSequence))
  }


  def getstats(h: Int): (LikelihoodRatioStatistic[CatCellT, OneColumnDataset[CatCellT]], PatternStatistic[CatCellT, OneColumnDataset[CatCellT]]) = {
    val pattern = new TrianglePattern(2 * h)
    val lrt = new LikelihoodRatioStatistic[CatCellT, OneColumnDataset[CatCellT]](model, h)
    val patt_lrt = new PatternStatistic[CatCellT, OneColumnDataset[CatCellT]](pattern, lrt)
    (lrt, patt_lrt)
  }


  val (lrt_20, pat_lrt_20) = getstats(500)

  val data = getWindDocuments

  //lrt_20.getValueWithLocations(data).foreach(println)


  val pl = new PlotXY("t", "LRT statistics")
  pl.addline(pat_lrt_20.getValueWithLocations(data), "h = 20")

  pl.print("plsa.pdf")

}
