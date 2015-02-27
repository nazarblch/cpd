package generate

import java.util.Random

import breeze.linalg.{normalize, DenseVector}



class MarkovVectorsGenerator(val first: DenseVector[Double], val delta: Double)
  extends ParameterGenerator[DenseVector[Double]] {

  val r = new Random()
  var current: DenseVector[Double] = first

  def getFractions: DenseVector[Double] = {
    val res = DenseVector.fill(first.length)(r.nextDouble())
    res :/ math.sqrt(res.foldLeft(0.0)((res, x) => res + x * x))
  }

  def getDirection: Int = {
    if(r.nextBoolean()) 1 else -1
  }

  def nextVector: DenseVector[Double] = {
    current = current + (getFractions :* (delta * getDirection))
    current
  }

  override def next: DenseVector[Double] = nextVector
}


class MarkovPositiveVectorsGenerator(val first: DenseVector[Double], val delta: Double)
  extends ParameterGenerator[DenseVector[Double]] {

  val r = new Random()
  var current: DenseVector[Double] = first

  assert(first.data.min > 0 && delta > 0)

  def getFractions: DenseVector[Double] = {
    val res = DenseVector.fill(first.length)(r.nextDouble())
    res :/ math.sqrt(res.foldLeft(0.0)((res, x) => res + x * x))
  }

  def getDirection: Int = {
    if(r.nextBoolean()) 1 else -1
  }

  def nextVector: DenseVector[Double] = {
    val dir = getDirection
    val next: DenseVector[Double] = current + (getFractions :* (delta * dir))
    if (next.data.min > 0) current = next else current = current + (getFractions :* delta)
    current
  }

  override def next: DenseVector[Double] = nextVector
}


class MarkovScalarGenerator(val first: Double, val delta: Double) extends ParameterGenerator[Double] {

  val r = new Random()
  var current: Double = first

  def getDirection: Int = {
    if(r.nextBoolean()) 1 else -1
  }

  def nextDouble: Double = {
    current = current + (delta * getDirection)
    current
  }

  override def next: Double = nextDouble
}

class MarkovPositiveScalarGenerator(val first: Double, val delta: Double) extends ParameterGenerator[Double] {

  assert(first > 0 && delta > 0)

  val r = new Random()
  var current: Double = first

  def getDirection: Int = {
    if(r.nextBoolean()) 1 else -1
  }

  def nextDouble: Double = {
    val next = current + (delta * getDirection)
    if (next > 0) current = next else current += delta
    current
  }

  override def next: Double = nextDouble

}


class MarkovMeanVarGenerator(val m: Double = 0, val s2: Double = 1, val delta: Double) extends ParameterGenerator[(Double, Double)] {

  assert(s2 > 0 && delta > 0)

  val r = new Random()
  var current: (Double, Double) = (m, s2)

  def getDirection: Int = {
    if(r.nextBoolean()) 1 else -1
  }

  val norm = math.sqrt(2)

  def nextVar: Double = {
    val next = current._2 + (delta * getDirection) / norm
    if (next > 1e-5) next else current._2 + delta / norm
  }

  def nextMean: Double = {
    current._1 + (delta * getDirection) / norm
  }

  override def next: (Double, Double) = {
    current = (nextMean, nextVar)
    current
  }

}


class MarkovProbabilityGenerator(val first: Double, val delta: Double) extends ParameterGenerator[Double] {

  assert(first > 0 && first < 1 && delta > 0 && delta < 0.5)

  val r = new Random()
  var current: Double = first

  def getDirection: Int = {
    if(r.nextBoolean()) 1 else -1
  }

  def nextDouble: Double = {
    val dir = getDirection
    val next = current + (delta * dir)
    if (next > 0 && next < 1) current = next else current -= (delta * dir)
    assert(current > 0 && current < 1)
    current
  }

  override def next: Double = nextDouble

}


