name := "shaqal"

organization := "org.shaqal"

version := "0.4.5.2"

scalaVersion := "3.3.3"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.2.10" % "test",
	"com.h2database" % "h2" % "1.3.170" % "test",
	"net.sourceforge.jtds" % "jtds" % "1.3.1" % "test",
	"org.postgresql" % "postgresql" % "9.4.1212" % "test", 
	"com.zaxxer" % "HikariCP" % "2.6.1" % "test",
	"org.slf4j" % "slf4j-simple" % "1.7.25" % "test"
)

parallelExecution in Test := false 

publishTo := Some("Vedaa Data Public publisher" at "https://mymavenrepo.com/repo/zPAvi2SoOMk6Bj2jtxNA/")


// workaround for bug "cannot take signature of MethodType" in doc task
Compile / doc / sources := Seq.empty
Compile / packageDoc / publishArtifact := false
