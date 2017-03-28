name := "shaqal"

organization := "org.shaqal"

version := "0.4-SNAPSHOT"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.10.4", "2.11.8", "2.12.1")

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.1" % "test",
	"com.h2database" % "h2" % "1.3.170" % "test",
	"net.sourceforge.jtds" % "jtds" % "1.3.1" % "test",
	"org.postgresql" % "postgresql" % "9.4.1212" % "test",
	"com.zaxxer" % "HikariCP" % "2.6.1" % "test",
	"org.slf4j" % "slf4j-simple" % "1.7.25"
)

parallelExecution in Test := false 

