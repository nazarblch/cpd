

import java.io.File

import cp_detectors.{OnlineChangePointDetector, ChangePointDetector}
import datasets.CellT._
import datasets.{DatasetLoader, DatasetConverter, Dataset}
import quality.QualityMeasure

import scala.collection.mutable
import scala.xml.NodeSeq

class TestConfiguration[T >: TCellDouble, D] (val tester: DetectorTester[T, D],
                                              val params: Map[String, Double]) {

}

class TestResult (val params: Map[String, Double], val value: Option[Double], val measureName: String) {}

class TestsManager[T >: TCellDouble, D <: ChangePointDetector[T]](val tests: Seq[TestConfiguration[T, D]]) {

  private val history: mutable.HashMap[String, Seq[TestResult]] = mutable.HashMap()

  private def runTest(conf: TestConfiguration[T, D], detector: D): Unit = {
    val res = conf.tester.run(detector).toSeq.map({case (measure, value) => new TestResult(detector.params ++ conf.params, value, measure)})
    history.update(detector.name, history.getOrElse(detector.name, Seq()) ++ res)
  }

  def testDetector(detector: D): Unit = {
    tests.foreach(conf => runTest(conf, detector))
  }

  def getPlot(measureName: String, detectorName: String, parameterName: String): Seq[(Double,Double)] = {
    history.get(detectorName).get.filter(tr => tr.measureName == measureName).filter(_.value.isDefined).
      map(tr => (tr.params.get(parameterName).get, tr.value.get))
  }

}


object TestDataLoader {

  class Directory(val path: String, val params: Map[String, Double]){}

  def parseXML(path: String): Seq[Directory] = {
    val node = scala.xml.XML.loadFile(path)

    (node \\ "directory").map { dir =>
      val path = (dir \ "path").text

      val params = (dir \ "parameter").map { param =>
        ((param \ "name").text.trim, (param \ "value").text.toDouble)
      }

      new Directory(path, params.toMap)
    }

  }

  def loadTestElementFromFile(path: String): TestElement[Double] = {

    val dataset: Dataset[Double] = DatasetConverter.toNumeric(DatasetLoader.loadFromFile(path + "/data.csv"))
    val reference: Vector[Int] = io.Source.fromFile(path + "/reference.csv").getLines().map(line => {
      if (line.trim.split(",")(0).toInt == 1) true else false
    }).toVector.zipWithIndex.filter(_._1).map(_._2)

    new TestElement[Double](dataset, reference)
  }

  def loadTestData(path: String): Seq[TestElement[Double]] = {

    new File(path).listFiles().filter(_.isDirectory).
      map(file => loadTestElementFromFile(file.getAbsolutePath))

  }

  def loadOnlineTestConfigurations(confPath: String, measures: Set[String]): Seq[TestConfiguration[Double, OnlineChangePointDetector[Double]]] = {
    val dirs = parseXML(confPath)
    dirs.map(dir => new TestConfiguration[Double, OnlineChangePointDetector[Double]](
      new OnlineDetectorTester[Double](loadTestData(dir.path), measures),
      dir.params
    ))
  }


}
