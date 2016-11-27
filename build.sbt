import SonatypeKeys._

sonatypeSettings

name := "cpd"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies  ++= Seq(
    // other dependencies here
    "org.scalanlp" %% "breeze" % "0.12",
    // native libraries are not included by default. add this if you want them (as of 0.7)
    // native libraries greatly improve performance, but increase jar sizes.
    // It also packages various blas implementations, which have licenses that may or may not
    // be compatible with the Apache License. No GPL code, as best I know.
    "org.scalanlp" %% "breeze-natives" % "0.12",
    // the visualization library is distributed separately as well.
    // It depends on LGPL code.
    "org.scalanlp" %% "breeze-viz" % "0.12"
)

resolvers ++= Seq(
    // other resolvers here
    // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
    "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

resolvers += "JCenter" at "http://jcenter.bintray.com/"



resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scala-lang" % "scala-xml" % "2.11.0-M4"

// https://mvnrepository.com/artifact/com.github.haifengl/smile-scala_2.11
libraryDependencies += "com.github.haifengl" % "smile-scala_2.11" % "1.2.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"


organization := "wias.de"

unmanagedJars in Compile ++=
  (file("/Users/buzun/cpd/lib/") * "*.jar").classpath


mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
{
    case PathList("org", "slf4j", xs @ _*) => MergeStrategy.last
    case PathList("javax", "activation", xs @ _*) => MergeStrategy.last
    case PathList("org", "apache", xs @ _*) => MergeStrategy.last
    case PathList("com", "google", xs @ _*) => MergeStrategy.last
    case PathList("com", "esotericsoftware", xs @ _*) => MergeStrategy.last
    case PathList("com", "codahale", xs @ _*) => MergeStrategy.last
    case PathList("com", "yammer", xs @ _*) => MergeStrategy.last
    case "log4j.properties" => MergeStrategy.last
    case x => old(x)
}
}



mainClass in assembly := Some("examples.QualityTest")

