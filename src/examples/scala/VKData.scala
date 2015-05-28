import viz.utils.PlotXY

object VKData extends App {

  def dayToVector(data: Vector[String], hoursInterval: Int = 2): (Vector[Double], Vector[Double])  = {

    val gsize = 1 + data.length / (24/hoursInterval)

    val first = data.sortBy(_.split(",")(4)).grouped(gsize).toVector.map(rows => rows.map(s => s.split(",")(1).toDouble * (1 - s.split(",").last.toDouble)).sum / rows.length)
    val second = data.sortBy(_.split(",")(4)).grouped(gsize).toVector.map(rows => rows.map(s => s.split(",").last.toDouble).sum / rows.length)

    assert(first.size == 24/hoursInterval)

    (first, second)
  }


  val format = new java.text.SimpleDateFormat("dd.MM.yyyy")
  val dataPerDay = io.Source.fromFile("/Users/nazar/cpd/data/vklog.txt").getLines().toVector.groupBy(s => s.split(",")(2).trim).toVector.
    sortBy({case (s,v) => format.parse(s).getTime})


  val dateToVec = dataPerDay.map({case(date, rows) => (date, dayToVector(rows)) })

  val table = dateToVec.flatMap({case(s, (v1, v2)) => v1.zip(v2).map(p => (s, p._1, p._2))})

  table.foreach(s => println(s._1 + "," + s._2 + "," + s._3))


  val pl = new PlotXY("t", "activity")

  pl.addline(table.map(_._2).toArray.slice(0, 500), "comp")

  pl.addline(table.map(_._3).toArray.slice(0, 500), "mob")

  pl.print("boom500.pdf")

}
