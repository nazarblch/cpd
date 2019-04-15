
name := "cpd"

version := "1.0"

scalaVersion := "2.12.7"

libraryDependencies  ++= Seq(
    // Last stable release
    "org.scalanlp" %% "breeze" % "0.13.2",

    // Native libraries are not included by default. add this if you want them (as of 0.7)
    // Native libraries greatly improve performance, but increase jar sizes. 
    // It also packages various blas implementations, which have licenses that may or may not
    // be compatible with the Apache License. No GPL code, as best I know.
    "org.scalanlp" %% "breeze-natives" % "0.13.2",

    // The visualization library is distributed separately as well.
    // It depends on LGPL code
    "org.scalanlp" %% "breeze-viz" % "0.13.2"
)


resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

resolvers += "JCenter" at "http://jcenter.bintray.com/"

// https://mvnrepository.com/artifact/org.bytedeco/javacpp
libraryDependencies += "org.bytedeco" % "javacpp" % "1.4.3"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.1.1"

// https://mvnrepository.com/artifact/com.github.haifengl/smile-scala_2.11
libraryDependencies += "com.github.haifengl" % "smile-scala_2.11" % "1.2.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"


resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

organization := "wias.de"

unmanagedJars in Compile ++=
  (file("lib/") * "*.jar").classpath



unmanagedJars in Compile += file("/home/nazar/torch_scala/target/scala-2.12/torch_scala_2.12-1.0.jar")




assemblyMergeStrategy in assembly := {
    case PathList("org", "slf4j", xs @ _*) => MergeStrategy.last
    case PathList("javax", "activation", xs @ _*) => MergeStrategy.last
    case PathList("org", "apache", xs @ _*) => MergeStrategy.last
    case PathList("com", "google", xs @ _*) => MergeStrategy.last
    case PathList("com", "esotericsoftware", xs @ _*) => MergeStrategy.last
    case PathList("com", "codahale", xs @ _*) => MergeStrategy.last
    case PathList("com", "yammer", xs @ _*) => MergeStrategy.last
    case PathList("org", "xmlpull", xs @ _*) => MergeStrategy.last
    case "log4j.properties" => MergeStrategy.last
    case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
}



mainClass in assembly := Some("examples.QualityTest")

