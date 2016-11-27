package models.glasso

import org.ddahl.jvmr.RInScala

/**
  * Created by buzun on 22/11/16.
  */
object GLassoInverse {

  def apply(X: Array[Array[Double]]): Array[Array[Double]] = {

    val R = RInScala()

    R.eval("library(glasso)")
    R.update("X", X)
    R.eval("R_covarianceMatrix = var(X)")
    R.eval("R_glasso = glasso(R_covarianceMatrix, rho=0.1)")
    val res = R.toMatrix[Double]("R_glasso$wi")

    R.quit()
    res
  }
}
