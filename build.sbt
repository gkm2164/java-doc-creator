name := "java-doc-creator"

version := "0.1"

scalaVersion := "2.12.8"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-M1"

scalacOptions ++= Seq("-deprecation", "-Ypartial-unification")

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

// if your project uses multiple Scala versions, use this for cross building
addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full)

// if your project uses both 2.10 and polymorphic lambdas
libraryDependencies ++= (scalaBinaryVersion.value match {
  case "2.10" =>
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full) :: Nil
  case _ =>
    Nil
})

libraryDependencies ++= Seq(
  "com.github.fomkin" %% "levsha-core" % "0.7.2",
  "org.planet42" %% "laika-core" % "0.10.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2")

libraryDependencies += "commons-io" % "commons-io" % "2.6"

mainClass in assembly := Some("co.gyeongmin.lang.javadoc.Main")
