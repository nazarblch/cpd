package cp_detectors

import bootstrap.{SmoothOnesGenerator, WeightedBootstrap, Bootstrap}
import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.stats.distributions.{Gaussian, MultivariateGaussian}
import datasets.{Column, DataHeader, OneColumnDataset, Dataset}
import models.ParametricModel
import models.standart.NormalModel
import patterns.{StaticHalfTrianglePattern, HalfTrianglePattern, TrianglePattern}
import statistics.{TailStatistic, MaxStatistic, PatternWeightedStatistic}
import statistics.likelihood_ratio.{WeightedStatisticFactory, SimpleWeightedLikelihoodRatioStatistic, WeightedLikelihoodRatioStatistic}

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer


class LRTOnlineDetector[Row, Self <: Dataset[Row, Self]](val model: ParametricModel[Row, Self, DenseVector[Double]],
                                                         val wstatFactory: WeightedStatisticFactory) extends OnlineChangePointDetector[Row, Self] {

  val WINDOW_SIZES: Array[Int] = Array(30, 50, 70)
  val CONFIDENCE = 0.1
  val BOOT_SIZE = 100


  ///////////

  val buf: ArrayBuffer[Row] = ArrayBuffer()
  val cp: ArrayBuffer[Boolean] = ArrayBuffer()
  val LRT: Map[Int, ArrayBuffer[Double]] = WINDOW_SIZES.map(h => (h, ArrayBuffer[Double]())).toMap
  val convs: Map[Int, ArrayBuffer[Double]] = WINDOW_SIZES.map(h => (h, ArrayBuffer[Double]())).toMap


//  def importRLib(path: String) {
//    R.eval(io.Source.fromFile(path).getLines().mkString("\n"))
//  }
//
//  def pushDataToR() {
//
//    R.update("inputVec", buf.toArray.flatten)
//    R.update("WH", WINDOW_SIZES)
//    R.eval("input <- matrix(inputVec, "+buf.last.length+", "+buf.length+")")
//
//    R.eval("InitMultiscaleCP(data = input,  H = WH)")
//  }

  override def addData(row: Row): Unit = {
    buf += row
//    if (buf.length == 2 * WINDOW_SIZES.max + 1) {
//      pushDataToR()
//    }
  }
//
//  override def hasNewChangePoint: Boolean = {
//    if (buf.length < 2 * WINDOW_SIZES.max + 1) {
//      cp += false
//    } else {
//      R.update("curElem", toRDataSeq(buf.last))
//      R.update("upper", upperBounds)
//      val R_out = R.toVector[Double]("MultipleCPCheck(curElem, x = "+buf.length+", H = WH, family.pa = '"+family+"', upper )").map(_.toInt)
//      assert(R_out.length == WINDOW_SIZES.length)
//
//      cp += R_out.zip(WINDOW_SIZES).map{case (s, h) =>
//        val head = math.min(3 * h, cp.length)
//        cp.takeRight(head).forall(!_) && s == 1
//      }.exists(x => x)
//
//    }
//    cp.last
//  }



  override def hasNewChangePoint: Boolean = {

      val dataset = new OneColumnDataset[Double](DataHeader(1), Column.apply[Double](buf.toVector.map(_.asInstanceOf[Double])), true).asInstanceOf[Self]
      val R_out = MultipleCPCheck(dataset)

      cp += R_out.map{case (h, s) =>
        val head = math.min(3 * h, cp.length)
        cp.takeRight(head).forall(!_) && s
      }.exists(x => x)


      cp.last
  }

  override def clear(): Unit = {
    buf.clear()
    cp.clear()
    // R.eval("data <- list()")
    LRT.foreach(_._2.clear())
    convs.foreach(_._2.clear())
  }

//  def toRDataSeq(dataset: Self): Array[Array[Double]] = {
//    dataset.getRowsIterator.toArray.map(_.toA)
//  }

  def toRDataSeq(row: IndexedSeq[Double]): Array[Double] = {
    row.toArray
  }

//  override def init(dataset: Dataset[Double]): Unit = {
//
//    assert(dataset.size > 4 * WINDOW_SIZES.max + 1)
//
//    R.update("inputVec", toRDataSeq(dataset).flatten)
//    R.update("WH", WINDOW_SIZES)
//    R.eval("input <- matrix(inputVec, "+dataset.dim+", "+dataset.size+")")
//
//    R.eval("BootstrapValues(input, family.pa = '"+family+"', H = WH, M = "+BOOT_SIZE+", alpha = "+CONFIDENCE+")")
//    // R.eval("BootstrapValues(input, family.pa = 'Gaussian', H = WH, M = 100, alpha = 0.03)")
//
//    upperBounds = R.toVector[Double]("BootstrapLine")
//
//    R.eval("BootstrapLine <- c()")
//
//    assert(upperBounds.length == WINDOW_SIZES.length)
//
//  }

  def Likelihood(data: Self): Double = {

    model.likelihood(data)

  }

  def MultipleCPCheck (data: Self): Seq[(Int, Boolean)] = {

    val out: mutable.HashMap[Int, Boolean] = mutable.HashMap()

    WINDOW_SIZES.foreach(h => out.update(h, false))

    WINDOW_SIZES.filter(_ <= data.size / 2).foreach{h =>

      val n = data.size
      val L1 = Likelihood(data.subset(n - 2 * h, n - h))
      val L2 = Likelihood(data.subset(n - h, n))
      val L = Likelihood(data.subset(n - 2 * h, n))

      if(L1 + L2 < L - 0.001){
        throw new Exception("incorrect stat value")
      }

      LRT.get(h).get += math.sqrt(2 * ( L1 + L2 - L + 0.001))

      if (LRT.get(h).get.length >=  h + 1){
        val stat = DenseVector(LRT.get(h).get.takeRight(h).toArray)

        val conv: Double = new HalfTrianglePattern(h).fitParameters(stat)

        //println(conv)
        convs.get(h).get += conv

        if(conv > configurations.filter(_.windowSize == h)(0).upperBound.get){
          out.update(h, true)
        }

      }
    }

    out.toSeq
  }

  override def name: String = "LRTOnline"

  class Config (val windowSize: Int,
                var upperBound: Option[Double],
                var maxDist: Option[TailStatistic]){}

  val configurations: Array[Config] = WINDOW_SIZES.map{h => new Config(h, None, None)}

  private def setMaxDist(config: Config, dataset: Self): Unit = {

    val wlrt = wstatFactory(model, config.windowSize)
    val pattern = new HalfTrianglePattern(config.windowSize)
    val patt_wlrt = new PatternWeightedStatistic[Row, Self](pattern, wlrt)
    val max_patt_wlrt = new MaxStatistic[Row, Self](patt_wlrt)

    val bootstrap: Bootstrap[Row, Self] = new WeightedBootstrap[Row, Self](new SmoothOnesGenerator, max_patt_wlrt)

    val sample = bootstrap.sample(dataset, BOOT_SIZE)

    config.maxDist = Some(new TailStatistic(sample.data))
  }

  private def setUpperBounds(dataset: Self): Unit = {

    for (config <- configurations.filter(_.maxDist.isDefined)) {
      config.upperBound = Some(config.maxDist.get.quantile(CONFIDENCE))
    }

  }

  override def init(dataset: Self): Unit = {

    clear()

    for (config <- configurations) {
      config.upperBound = None
      setMaxDist(config, dataset)
    }

    setUpperBounds(dataset)

  }
}
