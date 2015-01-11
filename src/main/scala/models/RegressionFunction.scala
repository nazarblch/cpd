package models

import breeze.linalg.DenseVector


trait RegressionFunction[T, P] {

  def value(row: IndexedSeq[T], param: P): Double

  def grad(row: IndexedSeq[T], param: P): DenseVector[Double]

}
