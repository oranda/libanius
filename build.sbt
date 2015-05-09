import sbt._

name := "Libanius"

scalaVersion := "2.11.6"

scalacOptions += "-deprecation"

resolvers ++= Seq("Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
                  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "http://oss.sonatype.org/content/repositories/releases",
                  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
                 )

libraryDependencies ++= Seq("com.typesafe.config" % "config" % "0.3.0",
                            "org.specs2" %% "specs2-core" % "2.4.17" % "test",
                            "org.specs2" %% "specs2-junit" % "2.4.17" % "test",
                            "org.scalaz" %% "scalaz-core" % "7.1.2",
                            "org.apache.httpcomponents" % "httpclient" % "4.1.2",
                            "com.typesafe.play" %% "play-json" % "2.4.0-RC1"
                           )

//seq(ScctPlugin.instrumentSettings : _*)
    
unmanagedClasspath in Runtime <+= (baseDirectory) map { bd => Attributed.blank(bd / "config") }

unmanagedClasspath in Test <+= (baseDirectory) map { bd => Attributed.blank(bd / "config") }

parallelExecution in Test := true

artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
  artifact.name + "-0.981." + artifact.extension
}

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"

exportJars := false

fork := false

javaOptions in run += "-XX:+UseConcMarkSweepGC"

javaOptions in run += "-XX:+CMSClassUnloadingEnabled"
 
javaOptions in run += "-XX:PermSize=512M" 

javaOptions in run += "-XX:MaxPermSize=512M"

