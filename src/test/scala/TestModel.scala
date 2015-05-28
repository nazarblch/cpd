import java.util.Random

import bootstrap.SmoothOnesGenerator
import breeze.linalg.DenseVector
import breeze.optimize.{ApproximateGradientFunction, DiffFunction, GradientTester}
import breeze.stats.distributions.Gaussian
import datasets._
import models.ParametricModel
import models.standart.NormalModel
import org.scalatest.FlatSpec
import statistics.likelihood_ratio.{ExtendedLikelihoodRatioStatistic, MeanVarWeightedLikelihoodRatioStatistic}


class TestModel  extends FlatSpec {


  def testF(f: DiffFunction[DenseVector[Double]], x0: DenseVector[Double]): Unit = {

    def g(x: DenseVector[Double]) = f.valueAt(x)

    val diffg = new ApproximateGradientFunction(g)

    val grad = f.gradientAt(x0)

    val dg: DenseVector[Double] = grad - diffg.gradientAt(x0)

    assert(dg.norm(1) / grad.norm(1)  < 0.001)
  }


  def test[Row, Self <: Dataset[Row, Self]](model: ParametricModel[Row, Self, DenseVector[Double]], data: Self, x0: DenseVector[Double]): Unit = {

    val f = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        (
          model.likelihood(data, x),
          model.gradLikelihood(data, x)
          )
      }
    }

    val wdata = WeightedDataset(data, Gaussian(1.0, 1.0).sample(data.size).toVector)

    val wf = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        (
          model.likelihood(wdata, x),
          model.gradLikelihood(wdata, x)
          )
      }
    }

    testF(f, x0)

    testF(wf, x0)

  }


  def testMLE(model: NormalModel, data: OneColumnDataset[Double]): Unit = {
    val opt = new MeanVarWeightedLikelihoodRatioStatistic(model, data.size / 2)
    val wdata = WeightedDataset(data, Gaussian(1.0, 1.0).sample(data.size).toVector)

    val sp_data = wdata.splitAt(wdata.size / 2)
    val mle = model.MLE(wdata)

    val x0 = DenseVector(0.0, 1.0)
    val (mle1, mle2) = opt.findMLEH0(sp_data, x0, x0)

    val diff1 = mle - mle1
    val diff2 = mle - mle2

    assert(diff1.norm(1) < 0.001)
    assert(diff2.norm(1) < 0.001)

    val grad = model.gradLikelihood(wdata, mle)

    assert(grad.norm(1) < 0.001)

  }

  def testFisherMatrix[Row, Self <: Dataset[Row, Self]](model: ParametricModel[Row, Self, DenseVector[Double]], data: Self, x0: DenseVector[Double]): Unit = {
    val stat = new ExtendedLikelihoodRatioStatistic[Row, Self](model, data.size / 2)
    val wdata = WeightedDataset(data, Vector.fill(data.size)(1.0))
    val sp_data = wdata.splitAt(wdata.size / 2)

    val (xi1, xi2) = stat.getXi1Xi2(0, wdata, x0, x0)

    val (xi1_sq, xi2_sq) = (xi1 dot xi1, xi2 dot xi2)

    val L1 = model.likelihood(sp_data._1)
    val L2 = model.likelihood(sp_data._2)
    val mle1 = model.MLE(sp_data._1)
    val mle2 = model.MLE(sp_data._2)

    val L1_0 = model.likelihood(sp_data._1, x0)
    val L2_0 = model.likelihood(sp_data._2, x0)

    val dL1 = 2.0 * (L1 - L1_0)
    val dL2 = 2.0 * (L2 - L2_0)

    assert(math.abs(xi1_sq - dL1) < 0.5)
    assert(math.abs(xi2_sq - dL2) < 0.5)

    val r = new Random()
    val wdata1 = WeightedDataset(data, new SmoothOnesGenerator().generateVector(wdata.size))

    val (wxi1, wxi2) = stat.getXi1Xi2(0, wdata1)

    val (wxi1_sq, wxi2_sq) = (wxi1 dot wxi1, wxi2 dot wxi2)

    val sp_data1 = wdata1.splitAt(wdata1.size / 2)

    val wL1 = model.likelihood(sp_data1._1)
    val wL2 = model.likelihood(sp_data1._2)
    val wL1_0 = model.likelihood(sp_data1._1, mle1)
    val wL2_0 = model.likelihood(sp_data1._2, mle2)

    val wdL1 = 2.0 * (wL1 - wL1_0)
    val wdL2 = 2.0 * (wL2 - wL2_0)

    assert(math.abs(wxi1_sq - wdL1) < 0.5)
    assert(math.abs(wxi2_sq - wdL2) < 0.5)

    val xi12 = stat.get_delta_xi_norm_with_bias(0, wdata1)

    val wL = model.likelihood(wdata1)

    val dL = math.sqrt(2.0) * math.sqrt(wL1 + wL2 - wL)

    assert(math.abs(dL - xi12) < 0.5)

  }

  "A Normal model" should "be consistent in gradient and value" in {
    val col: Column[Double] = Column[Double](Gaussian(2,5).sample(100))
    val data = new OneColumnDataset[Double](DataHeader(1), col, true)
    val model = new NormalModel

    test(model, data, DenseVector(1.0,1.0))
    test(model, data, DenseVector(0.5,2.3))
    test(model, data, DenseVector(0.1,0.1))

  }

  it should "be consistent in defined and optimized MLE" in {
    val col: Column[Double] = Column[Double](Gaussian(2,5).sample(100))
    val data = new OneColumnDataset[Double](DataHeader(1), col, true)
    val model = new NormalModel

    testMLE(model, data)
  }

  it should "be consistent in Wilks equation" in {
    val r = new Random()
    val col: Column[Double] = Column[Double](Array.fill(10000)(2.0 * r.nextGaussian() + 2.0))
    val data = new OneColumnDataset[Double](DataHeader(1), col, true)
    val model = new NormalModel
    testFisherMatrix(model, data, DenseVector(2.0, 4.0))
  }



}
