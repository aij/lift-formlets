name := "lift-formlets"

organization := "gov.wicourts"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.4"

resolvers += "Sonatype Snapshots Repository" at "http://oss.sonatype.org/content/repositories/snapshots"

{
  val liftVersion = "3.0-M3"
  libraryDependencies ++= Seq(
    "net.liftweb" %% "lift-webkit" % liftVersion,
    "org.mortbay.jetty" % "jetty" % "6.1.22" % "test",
    "org.scalaz" %% "scalaz-core" % "7.1.1" % "compile"
  )
}

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "2.4.16" % "test"
)

scalacOptions ++= Seq("-deprecation","-feature","-Xfatal-warnings")

//scalacOptions in Test ++= Seq("-Yrangepos")

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(Path.userHome / ".sonatype")
