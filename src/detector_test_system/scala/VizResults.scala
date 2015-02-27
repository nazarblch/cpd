import java.io.{FileWriter, File}

import viz.utils.PlotXY

import scala.xml.{XML, Node}

class VizResults(measure: String, param: String, family: String, resDir: String = "testres_offline") {

  val pl = new PlotXY(param, measure)
  pl.p.ylim(0.0, 1.0)
  var maxY = 1.0

  def resPath(detector: String): String = {
    resDir +"/"+ detector + ".xml"
  }

  def loadResults(detector: String): Seq[TestResult] = {

    val res = XML.loadFile(resPath(detector)) \\ "result"
    res.map(node => TestResult(node)).filter(_.params.get("family").get.toString equals family).filter(_.measureName equals measure)
  }


  def addLine(detector: String): Unit = {

    val res = loadResults(detector)
    val X = res.map(_.params.get(param).get.toString.toDouble).toArray
    val Y = res.map(_.value.get).toArray

    if (Y.max > maxY) {
      pl.p.ylim(0.0, Y.max + 1)
      maxY = Y.max
    }

    pl.addline(X, Y, detector)
  }



  def plot(detectors: Seq[String], suf: String = "") = {

    detectors.foreach(addLine)
    pl.print(resDir + "/img/" + measure + "_" + param + "_" + family + ".pdf")
  }

}
