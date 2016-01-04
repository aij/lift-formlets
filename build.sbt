name := "lift-formlets"

organization := "gov.wicourts"

version := "0.2.0"

scalaVersion := "2.11.6"

resolvers += "Sonatype Snapshots Repository" at "http://oss.sonatype.org/content/repositories/snapshots"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

{
  val liftVersion = "3.0-M3"
  libraryDependencies ++= Seq(
    "net.liftweb" %% "lift-webkit" % liftVersion,
    "org.mortbay.jetty" % "jetty" % "6.1.22" % "test",
    "org.scalaz" %% "scalaz-core" % "7.1.1" % "compile"
  )
}

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.0" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "3.0" % "test"
)

scalacOptions ++= Seq("-deprecation","-feature","-Xfatal-warnings")

//scalacOptions in Test ++= Seq("-Yrangepos")

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

