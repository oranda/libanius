import sbt._

name := "Libanius"

scalaVersion := "2.10.2"

scalacOptions += "-deprecation"

resolvers ++= Seq("Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
          "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
		  "releases"  at "http://oss.sonatype.org/content/repositories/releases")

libraryDependencies ++= Seq("com.typesafe.config" % "config" % "0.3.0",
                            "org.specs2" %% "specs2" % "2.1",
                            "org.scalaz" %% "scalaz-core" % "7.0.3")
    
unmanagedClasspath in Runtime <+= (baseDirectory) map { bd => Attributed.blank(bd / "config") }

unmanagedClasspath in Test <+= (baseDirectory) map { bd => Attributed.blank(bd / "config") }

parallelExecution in Test := true

artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
  artifact.name + "-0.93." + artifact.extension
}

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"

exportJars := false

fork := false

javaOptions in run += "-XX:+UseConcMarkSweepGC"

javaOptions in run += "-XX:+CMSClassUnloadingEnabled"
 
javaOptions in run += "-XX:PermSize=512M" 

javaOptions in run += "-XX:MaxPermSize=512M"

