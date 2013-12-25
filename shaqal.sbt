name := "shaqal"

organization := "org.shaqal"

version := "0.5-SNAPSHOT"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "2.0" % "test",
	"com.h2database" % "h2" % "1.3.170" % "test",
	"net.sourceforge.jtds" % "jtds" % "1.2.4" % "test"
)

parallelExecution in Test := false 

