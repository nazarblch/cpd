package utils


import breeze.generic.UFunc
import breeze.linalg._
import breeze.linalg.operators.OpDiv



object sqrt extends UFunc {
  implicit object implDouble extends Impl[DenseMatrix[Double], DenseMatrix[Double]] {
    def apply(A: DenseMatrix[Double]) = {
      val res = eig(A)
      val eV = res.eigenvectors
      eV * diag(res.eigenvalues.map(lambla => math.sqrt(lambla))) * inv(eV)
    }
  }
}