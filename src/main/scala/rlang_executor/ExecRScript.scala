package rlang_executor

import breeze.stats.distributions.Gaussian
import org.ddahl.jvmr.RInScala

object ExecRScript extends App {

  val R = RInScala()

  def importRLib(path: String) {
    R.eval(io.Source.fromFile(path).getLines().mkString("\n"))
  }



  importRLib("src/R/meanvar.r")

//  R.eval(
//    """
//      |input <- matrix(c(rnorm(200, 10, 10), rnorm(200, 20, 10)), 2, 200)
//      |
//      |WH <- c(5, 10, 15)
//      |
//      |BootstrapValues(input[ ,1:80], family.pa = 'Gaussian',
//      |                H = WH, M = 10, alpha = .03)
//      |
//      |InitMultiscaleCP(data = input[ , 1:31 ],  H = WH)
//      |
//      |
//    """.stripMargin)
//
//  println(R.toMatrix[Double]("input").length +","+ R.toMatrix[Double]("input")(0).length)
//  println(R.toVector[Double]("input[,1]").length)
//  println(R.capture("input[,1]"))
//
//  val upper = R.toVector[Double]("BootstrapLine")
//  R.update("upper", upper)
//
//  for (i <- 31 to 199) {
//    println(i.toString + ": " + R.capture("MultipleCPCheck(input[, "+i+"], x = "+i+", H = WH, family.pa = 'Gaussian', upper )"))
//  }

  R.update("data", (Gaussian(2,1).sample(100) ++ Gaussian(6,1).sample(100)).toArray)


  var res: Array[Int] = Array()


    res = R.toVector[Int]("cpLocations(data, 'PELT', 'Normal')").map(_.toInt)


  println(res.mkString(","))

}
