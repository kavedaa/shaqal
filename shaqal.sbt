name := "shaqal"

organization := "org.shaqal"

version := "0.4-SNAPSHOT"

scalaVersion := "2.11.12"

crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.8")

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.1" % "test",
	"com.h2database" % "h2" % "1.3.170" % "test",
	"net.sourceforge.jtds" % "jtds" % "1.3.1" % "test",
	"org.postgresql" % "postgresql" % "9.4.1212" % "test",
	"com.zaxxer" % "HikariCP" % "2.6.1" % "test",
	"org.slf4j" % "slf4j-simple" % "1.7.25" % "test"
)

parallelExecution in Test := false 

