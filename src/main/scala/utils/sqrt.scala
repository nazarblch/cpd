package utils


import breeze.generic.UFunc
import breeze.linalg._
import breeze.linalg.operators.OpDiv

object sqrt extends UFunc {
  implicit def fromNormAndDivide[Arg](implicit eigImpl: eig.Impl[Arg, (DenseVector[Double], DenseVector[Double], DenseMatrix[Double])]): Impl[Arg, DenseMatrix[Double]] = new Impl[Arg, DenseMatrix[Double]] {
    def apply(A: Arg) = {
      val res = eigImpl(A)
      res._3 * diag(res._1.map(lambla => math.sqrt(lambla))) * inv(res._3)
    }
  }
}