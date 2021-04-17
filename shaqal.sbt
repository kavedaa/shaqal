name := "shaqal"

organization := "org.shaqal"

version := "0.4.4"

scalaVersion := "3.0.0-RC1"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.2.5" % "test",
	"com.h2database" % "h2" % "1.3.170" % "test",
	"net.sourceforge.jtds" % "jtds" % "1.3.1" % "test",
	"org.postgresql" % "postgresql" % "9.4.1212" % "test",
	"com.zaxxer" % "HikariCP" % "2.6.1" % "test",
	"org.slf4j" % "slf4j-simple" % "1.7.25" % "test"
)

parallelExecution in Test := false 

publishTo := Some("My Maven Repo" at "https://mymavenrepo.com/repo/j1YxfckeUitD5ZGTAisl")
