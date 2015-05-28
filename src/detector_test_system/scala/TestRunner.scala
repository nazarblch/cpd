

import java.io.{FileWriter, File}

import cp_detectors.{OfflineChangePointDetector, OnlineChangePointDetector, ChangePointDetector}
import datasets.CellT._
import datasets._
import quality.QualityMeasure

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.xml._

class TestConfiguration[Row, Self <: Dataset[Row, Self], D <: ChangePointDetector[Row, Self]] (
                                              val tester: DetectorTester[Row, Self, D],
                                              val params: Map[String, Any]) {

  override def toString: String = "config: " + params.toString()

  def corresponds(res: TestResult): Boolean = {
    val paramEq = params.toSeq.sortBy(_._1).mkString(",") equals res.params.toSeq.sortBy(_._1).mkString(",")
    paramEq
  }

}

class TestResult (val params: Map[String, Any], val value: Option[Double], val measureName: String) {
  def toXML: Node =
    <result>
      <measure>{measureName}</measure>
      <value>{value.getOrElse("")}</value>
      <params>
      {params.toSeq.map(p => {p._1 + "=" + p._2}).mkString(",")}
      </params>
    </result>
}

object TestResult {

  def apply(node: Node): TestResult = {
    val params: Map[String, Any] = (node \ "params").text.trim.split(",").map(p => (p.split("=")(0), p.split("=")(1))).toMap
    new TestResult(params, Some((node \ "value").text.toDouble), (node \ "measure").text)
  }

}



class TestsManager[Row, Self <: Dataset[Row, Self], D <: ChangePointDetector[Row, Self]](val tests: Seq[TestConfiguration[Row, Self, D]],
                                                                  val resDir: String) {

  private val history: mutable.HashMap[String, Seq[TestResult]] = mutable.HashMap()

  def resPath(detector: D): String = {
    resDir +"/"+ detector.name + ".xml"
  }

  def loadResults(detector: D): Seq[Node] = {

    val file = new File(resPath(detector))
    if (! file.exists() ) {
      val fw = new FileWriter(resPath(detector))
      fw.write("<root> \n " )
      fw.write("</root> \n " )
      fw.close()
    }

    val res = XML.loadFile(resPath(detector)) \\ "result"
    // results.update(detector.name, ArrayBuffer() ++ res)
    res
  }

  def updateResults(detector: D, results: Seq[TestResult]): Unit = {

    results.foreach(res => println("write res: " + res.toXML))
    val loaded = loadResults(detector)

    val fw = new FileWriter(resPath(detector), false)
    fw.write("<root> \n " )
    (loaded ++ results.map(_.toXML)).foreach(node => fw.write("\n" + node.toString() + "\n"))
    fw.write("</root> \n " )
    fw.flush()
    fw.close()
  }


  private def runTest(conf: TestConfiguration[Row, Self, D], detector: D): Unit = {
    val res = conf.tester.run(detector).toSeq.map({case (measure, value) => new TestResult(detector.params ++ conf.params, value, measure)})
    println(res.map(tr => tr.measureName + "=" + tr.value.getOrElse("")).mkString(", "))
    updateResults(detector, res)
    history.update(detector.name, history.getOrElse(detector.name, Seq()) ++ res)
  }

  def testDetector(detector: D, retest: Boolean): Unit = {
    tests.foreach(conf => {
      println(conf.toString)
      if (retest) {
        val fw = new FileWriter(resPath(detector))
        fw.write("<root> \n " )
        fw.write("</root> \n " )
        fw.close()
      }
      val isTested = loadResults(detector).exists(node => conf.corresponds(TestResult(node)))
      if (!isTested || retest) runTest(conf, detector)
      else println("already tested")
    })
  }

  def getPlot(measureName: String, detectorName: String, parameterName: String): Seq[(Double,Double)] = {
    history.get(detectorName).get.filter(tr => tr.measureName == measureName).filter(_.value.isDefined).
      map(tr => (tr.params.get(parameterName).get.toString.toDouble, tr.value.get))
  }

}


object TestDataLoader {

  class Directory(val path: String, val params: Map[String, String]){}

  def parseXML(path: String): Seq[Directory] = {
    val node = scala.xml.XML.loadFile(path)

    (node \\ "directory").map { dir =>
      val path = (dir \ "path").text

      val params = (dir \ "parameter").map { param =>
        ((param \ "name").text.trim, (param \ "value").text)
      }

      new Directory(path, params.toMap)
    }

  }

  def loadTestElementFromFile(path: String): TestElement[Double, OneColumnDataset[Double]] = {

    val dataset: OneColumnDataset[Double] = DatasetConverter.toNumeric(DatasetLoader.loadFromFile(path + "/data.csv"))
    val reference: Vector[Int] = io.Source.fromFile(path + "/reference.csv").getLines().map(line => {
      if (line.trim.split(",")(0).toInt == 1) true else false
    }).toVector.zipWithIndex.filter(_._1).map(_._2)

    new TestElement[Double, OneColumnDataset[Double]](dataset, reference)
  }

  def loadTestData(path: String): Seq[TestElement[Double, OneColumnDataset[Double]]] = {

    new File(path).listFiles().filter(_.isDirectory).
      map(file => loadTestElementFromFile(file.getAbsolutePath))

  }

  def parse(s: String): Option[Double] = try { Some(s.toDouble) } catch {case _ => None}

  def loadOnlineTestConfigurations(confPath: String,
                                   family: String,
                                   measures: Set[String]): Seq[TestConfiguration[Double, OneColumnDataset[Double], OnlineChangePointDetector[Double, OneColumnDataset[Double]]]] = {
    val dirs = parseXML(confPath).filter(_.params.get("family").get.trim equals family)
    dirs.map(dir => new TestConfiguration[Double, OneColumnDataset[Double], OnlineChangePointDetector[Double, OneColumnDataset[Double]]](
      new OnlineDetectorTester[Double, OneColumnDataset[Double]](loadTestData(dir.path), measures),
      dir.params.map({case (k,v) => (k, parse(v).getOrElse(v))})
    ))
  }

  def loadOfflineTestConfigurations(confPath: String,
                                   family: String,
                                   measures: Set[String]): Seq[TestConfiguration[Double, OneColumnDataset[Double], OfflineChangePointDetector[Double, OneColumnDataset[Double]]]] = {
    val dirs = parseXML(confPath).filter(_.params.get("family").get.trim equals family)
    dirs.map(dir => new TestConfiguration[Double, OneColumnDataset[Double], OfflineChangePointDetector[Double, OneColumnDataset[Double]]](
      new OfflineDetectorTester[Double, OneColumnDataset[Double]](loadTestData(dir.path), measures),
      dir.params.map({case (k,v) => (k, parse(v).getOrElse(v))})
    ))
  }


}
