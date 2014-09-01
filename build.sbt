import AssemblyKeys._

assemblySettings

jarName in assembly := "Clock.jar"

name := "PGHU"

version := "1.0"

scalaVersion := "2.11.0"

scalacOptions ++= Seq("-deprecation")
  
sbtVersion := "0.13.2"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.5" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.1"

