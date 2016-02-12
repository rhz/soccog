name := "soccog"

organization := "uk.ac.ed.inf"

version := "0.1"

scalaVersion := "2.11.7"

sbtVersion := "0.13.+"

scalacOptions ++= Seq("-deprecation", "-feature")

resolvers += "Sonatype snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots/"

// val scalajs = false
// if (scalajs) {
//   enablePlugins(ScalaJSPlugin);
// }

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.0.+" % "test")
