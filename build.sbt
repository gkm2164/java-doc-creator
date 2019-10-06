name := "java-doc-creator"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-M1"

scalacOptions ++= Seq("-deprecation")

libraryDependencies ++= Seq(
  "com.github.fomkin" %% "levsha-core" % "0.7.2",
  "org.planet42" %% "laika-core" % "0.10.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2")

mainClass in assembly := Some("co.gyeongmin.lang.javadoc.Main")
