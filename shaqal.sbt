name := "shaqal"

organization := "org.shaqal"

version := "0.4-SNAPSHOT"

scalaVersion := "2.11.5"

crossScalaVersions := Seq("2.10.4", "2.11.5")

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "2.2.1" % "test",
	"com.h2database" % "h2" % "1.3.170" % "test",
	"net.sourceforge.jtds" % "jtds" % "1.3.1" % "test"
)

parallelExecution in Test := false 

