package models

import breeze.linalg.DenseVector


trait RegressionFunction[Row, P] {

  def value(row: Row, param: P): Double

  def grad(row: Row, param: P): DenseVector[Double]

}
