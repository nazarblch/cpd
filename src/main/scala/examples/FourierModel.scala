package examples


import breeze.stats.distributions.Poisson
import torch_scala.api.aten.{CUDA, Shape, Tensor}
import torch_scala.autograd.Variable
import torch_scala.nn.Linear
import torch_scala.optim.Adam
import torch_scala.autograd.MathVariable._
import viz.utils.PlotXY


class FourierNet(size: Int) {

  val fc1: Linear[Double, CUDA] = Linear[Double, CUDA](1, size)
  val fc2: Linear[Double, CUDA] = Linear[Double, CUDA](1, 5)
  val c1 = Variable(Tensor.randn[Double, CUDA](Shape(size, 1)))
  val c2 = Variable(Tensor.randn[Double, CUDA](Shape(5, 1)).abs())
  val c = Variable(Tensor.randn[Double, CUDA](Shape(1, 1)) + 1)
  val sigma = Variable(Tensor.arange[Double, CUDA](-6, -1).reshape(Shape(1, 5)))

  val optimizer: Adam[CUDA] = Adam[CUDA]((fc1.parameters ++ fc2.parameters ++ Seq(c1, c2, c, sigma)).asInstanceOf[Seq[Variable[Any, CUDA]]], 0.003)

  def forward(x: Variable[Double, CUDA]): Variable[Double, CUDA] = {
    (fc1(x) * 100).cos().mm(c1 / size) + c +
      ((fc2(x) * 10).cos().pow(2) * sigma ).exp().mm(c2 / 10)
  }

  def weighted_mse(y1: Variable[Double, CUDA], y2: Variable[Double, CUDA], weights: Variable[Double, CUDA]): Variable[Double, CUDA] = {
    val lseq = (y1 - y2).abs() + (y1 - y2).pow(2)
    lseq dot weights
  }

  def l1_pen(): Variable[Double, CUDA] = {
    (c1.abs().mean() * 0.5) + (c2.abs().mean() * 0.1)
  }

  def diff_matrix(n: Int): Tensor[Double, CUDA] = {
    val data =Array.range(0, n - 1).flatMap(i => {
      val row = Array.fill(n)(0d)
      row(i) = 1d
      row(i + 1) = -1d
      row
    })
    Tensor[Double, CUDA](data).reshape(Shape(n - 1, n))
  }

  def grad_pen(y: Variable[Double, CUDA], diff_matrix: Tensor[Double, CUDA]): Variable[Double, CUDA] = {
    Variable(diff_matrix).mm(y).abs().mean() / 20
  }

  def train(x: Array[Double], y: Array[Double], weights: Array[Double], itersCount: Int): (Double, Array[Double]) = {

    val n = y.length

    val xs = Tensor[Double, CUDA](x).reshape(Shape(y.length, 1)) / 100
    val ys = Variable( Tensor[Double, CUDA](y) )
    val weights = Variable(Tensor.ones_like(ys.data))
    val dM = diff_matrix(n)

    for (iter <- 0 to itersCount) {
      val y_pred = forward(Variable(xs))
      val loss = weighted_mse(ys, y_pred.reshape(n), weights) + l1_pen() + grad_pen(y_pred, dM)
      optimizer.zeroGrad()
      loss.backward()
      optimizer.step()
      // println(loss.data.cpu().item())
    }

    val y_pred = forward(Variable(xs))
    val loss = weighted_mse(ys, y_pred.reshape(n), weights)
    (loss.data.cpu().item(), y_pred.data.cpu().data())
  }

}


object DataLoader {
  def load_ecg(path: String): Array[Array[Double]] = {
    scala.io.Source.fromFile(path).getLines.toArray.map(line => {
      line.split(",").map(_.toDouble)
    })
  }
}


object LRT {

  val rand = Poisson(1.0)

  def gen_weights(h: Int): Array[Double] = {
    rand.sample(h).toArray.map(_.toDouble)
  }

  def apply(y: Array[Double], h: Int): Array[Double] = {

    val ws: Array[Double] = gen_weights(y.length)
    val xs: Array[Double] = Array.range(0, y.length).map(_.toDouble)

    val y_slides = y.sliding(2*h, 5)
    val w_slides = ws.sliding(2*h, 5)
    val x_slides = xs.sliding(2*h, 5)

    val model12 = new FourierNet(20)
    val model1 = new FourierNet(20)
    model12.train(xs.slice(0, 2*h), y.slice(0, 2*h), gen_weights(2*h), 1000)
    model1.train(xs.slice(0, h), y.slice(0, h), gen_weights(h), 1000)
    model1.train(xs.slice(h, 2*h), y.slice(h, 2*h), gen_weights(h), 1000)

    y_slides.zip(x_slides).zip(w_slides).toArray.map({case((y12, x12), w12) =>
      val y1 = y12.slice(0, h)
      val y2 = y12.slice(h, 2 * h)

      val x1 = x12.slice(0, h)
      val x2 = x12.slice(h, 2 * h)

      val w1 = w12.slice(0, h)
      val w2 = w12.slice(h, 2 * h)

//      val model1 = new FourierNet(100)
//      val model2 = new FourierNet(100)


      val (loss12, y_pred12) = model12.train(x12, y12, w12, 5)
      val (loss1, y_pred1) = model1.train(x1, y1, w1, 50)
      val (loss2, y_pred2) = model1.train(x2, y2, w2, 50)

      val pl = new PlotXY("time", "data")

      pl.addline(x12, y12, "ref")
      pl.addline(x12, y_pred12, "pred12")
      pl.addline(x1, y_pred1, "pred1")
      pl.addline(x2, y_pred2, "pred2")

      val lrt = loss12 - loss1 - loss2

      println(lrt)

      lrt
    })

  }
}


object FourierModel extends App {

  val net = new FourierNet(50)

  val data = DataLoader.load_ecg("/home/nazar/PycharmProjects/torch/data/ptbdb_normal.csv")

  //val (loss, y_pred) = net.train(data(10).slice(10, 140) ++ data(10).slice(10, 140), Array.fill(130 * 2)(1.0d), 1000)


  val row = data(10).slice(0, 105)
  val row1 = data(12).slice(0, 105)

  val rows =  row ++ row ++ row ++ row ++ row1 ++ row1


  val lrt = LRT.apply(rows, row.length + 50)


  val pl = new PlotXY("time", "LRT")
  pl.addline(lrt, "model interpolation")
  //pl.addline(y_pred.map(_.toDouble), "123")
  //pl.addline(data(10).slice(10, 140) ++ data(10).slice(10, 140), "123")





}