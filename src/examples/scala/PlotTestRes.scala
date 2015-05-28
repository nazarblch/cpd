/**
 * Created by nazar on 19/02/15.
 */
object PlotTestRes extends App {

  val fam = "N"

  val viz1 = new VizResults("Precision", "delta", fam)

  viz1.plot(Seq("LRTOnline", "BOCPD", "RMeanVar"))

  val viz2 = new VizResults("Recall", "delta", fam)

  viz2.plot(Seq("LRTOnline", "BOCPD", "RMeanVar"))

  val viz3 = new VizResults("Delay", "delta", fam)

  viz3.plot(Seq("LRTOnline", "BOCPD", "RMeanVar"))

}


object PlotTestResOffline extends App {

  val fam = "Po"

  val viz1 = new VizResults("NMI", "delta", fam)

  viz1.plot(Seq("LRTOffline", "BOCPD", "RMeanVar"))



}