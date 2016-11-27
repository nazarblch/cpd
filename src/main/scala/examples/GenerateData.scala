package examples

import java.io.{File, FileWriter}

import datasets.{Dataset, MultiColumnDataset}
import generate.{BinarySignalsGenerator, DataGenerator, HeterogeneousDataGenerator}

object GenerateData extends App {

  val size = 100
  val kant = 120
  val delta = 0.25
  val p = 1
  val interval = 50
  val family = DataGenerator.POISSON
  val dir = "testdataonline"
  val sub_dir = dir + "/family_" + family + "_size_" + size + "_delta_" + delta + "_p_" + p

  val DATA_NAME = "data.csv"
  val REF_NAME = "reference.csv"

  BinarySignalsGenerator.maxCount = 1

  val xml_line =
    <directory>
      <path>{sub_dir}</path>
      <parameter>
        <name>family</name>
        <value>{family}</value>
      </parameter>
      <parameter>
        <name>size</name>
        <value>{size}</value>
      </parameter>
      <parameter>
        <name>delta</name>
        <value>{delta}</value>
      </parameter>
      <parameter>
        <name>p</name>
        <value>{p}</value>
      </parameter>
    </directory>


  writeXml(dir + "/config.xml")

  def writeXml(path: String): Unit = {

    val node = scala.xml.XML.loadFile(path)

    val isWritten = (node \\ "directory").map (dir => {
      (dir \ "path").text
    }).exists(s => s.trim equals sub_dir)

    if(!isWritten) {
      val fw = new FileWriter(path, false)
      fw.write("<dirs> \n " )
      fw.write("\n" + xml_line.toString() + "\n")
      (node \\ "directory").foreach(dir => fw.write("\n" + dir.toString() + "\n"))
      fw.write("</dirs> \n " )
      fw.close()
    }

  }


  def writeRef(signals: Array[Boolean], path: String): Unit = {
    val fw = new FileWriter(path)

    signals.foreach(s => {
      val line = if(s) 1 else 0
      fw.write(line + "\n")
    })

    fw.close()
  }

  def write(signals: Array[Boolean], data: MultiColumnDataset[Double]): Unit = {
    val file = new File(sub_dir)

    val num: Int =
      if (! file.exists()) {
        file.mkdir()
        0
      } else {
        file.list().length
      }

    new File(sub_dir + "/" + num).mkdir()

    val data_path = sub_dir + "/" + num + "/" + DATA_NAME
    data.save(data_path)

    val ref_path = sub_dir + "/" + num + "/" + REF_NAME
    writeRef(signals, ref_path)

  }

  val gen = HeterogeneousDataGenerator(kant, delta, p, interval, family)

  val (signals, data) = gen.draw(size)

 // write(signals, Dataset.applyCast(data))



}
