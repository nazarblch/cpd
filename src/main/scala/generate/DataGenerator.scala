package generate

import breeze.linalg.DenseVector


trait DataGenerator[T, P] {
  def next: T
  def update(param: P): Unit
}

trait ParameterGenerator[P] {
  def next: P
}

object DataGenerator {
  val NORMAL = "N"
  val MULTI_NORMAL = "MN"
  val DIRICHLET = "Dir"
  val POISSON = "Po"
  val BERNOULLI = "Be"

  def apply[T, P](p: Int,  family: String): DataGenerator[T, P] = family match {
    case NORMAL => new ScalarNormalDataGenerator().asInstanceOf[DataGenerator[T, P]]
    case MULTI_NORMAL => new NormalDataGenerator(DenseVector.zeros(p)).asInstanceOf[DataGenerator[T, P]]
    case DIRICHLET => new DirichletDataGenerator(DenseVector.ones(p)).asInstanceOf[DataGenerator[T, P]]
    case POISSON => new PoissonDataGenerator(1).asInstanceOf[DataGenerator[T, P]]
    case BERNOULLI => new BernoulliDataGenerator(0.5).asInstanceOf[DataGenerator[T, P]]
  }

}


object ParameterGenerator {

  implicit def apply[P](delta: Double,  p: Int,  family: String): ParameterGenerator[P] = family match {
    case DataGenerator.NORMAL => new MarkovMeanVarGenerator(delta = delta).asInstanceOf[ParameterGenerator[P]]
    case DataGenerator.MULTI_NORMAL => new MarkovVectorsGenerator(DenseVector.zeros(p), delta).asInstanceOf[ParameterGenerator[P]]
    case DataGenerator.DIRICHLET => new MarkovPositiveVectorsGenerator(DenseVector.ones(p), delta).asInstanceOf[ParameterGenerator[P]]
    case DataGenerator.POISSON => new MarkovPositiveScalarGenerator(1, delta).asInstanceOf[ParameterGenerator[P]]
    case DataGenerator.BERNOULLI => new MarkovProbabilityGenerator(0.5, delta).asInstanceOf[ParameterGenerator[P]]
  }
}

class HeterogeneousDataGenerator[T, P](averageInterval: Int,
                                       kant: Int,
                                       dataGen: DataGenerator[T, P],
                                       paramGen: ParameterGenerator[P]) {

  def draw(size: Int): (Array[Boolean], Vector[T]) = {
    val signals: Array[Boolean] = BinarySignalsGenerator.draw(size, averageInterval, kant)

    dataGen.update(paramGen.next)

    val data: Vector[T] = signals.toVector.map(isCP => {
      if (isCP) {
        dataGen.update(paramGen.next)
      }
      dataGen.next
    })

    (signals, data)
  }

}

object HeterogeneousDataGenerator {
  implicit def apply[T, P](kant: Int, delta: Double,  p: Int, interval: Int, family: String) = {
    new HeterogeneousDataGenerator[T, P](interval, kant, DataGenerator(p, family), ParameterGenerator(delta, p, family))
  }
}
