import sbt._

name := "libanius"

version := "0.984"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-unchecked", "-deprecation")

resolvers ++= Seq("Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
                  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "http://oss.sonatype.org/content/repositories/releases",
                  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
                 )

val scalazVersion = "7.1.2"

libraryDependencies ++= Seq("com.typesafe.config" % "config" % "0.3.0" % "provided",
                            "org.specs2" %% "specs2-core" % "2.4.17" % "test",
                            "org.specs2" %% "specs2-junit" % "2.4.17" % "test",
                            "org.scalaz" %% "scalaz-core" % scalazVersion,
                            "org.apache.httpcomponents" % "httpclient" % "4.1.2",
                            "com.typesafe.play" %% "play-json" % "2.4.0-RC1",
                            "com.lihaoyi" %% "fastparse" % "0.3.7"
                           )

unmanagedClasspath in Runtime <+= baseDirectory map { bd => Attributed.blank(bd / "config") }

unmanagedClasspath in Test <+= baseDirectory map { bd => Attributed.blank(bd / "config") }

parallelExecution in Test := true

artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
  artifact.name + "-" + version + "." + artifact.extension
}

assemblyJarName in assembly := s"${name.value}-${version.value}-fat.jar"

assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false)

test in assembly := {}

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"

exportJars := false

fork := false

javaOptions in run += "-XX:+UseConcMarkSweepGC"

javaOptions in run += "-XX:+CMSClassUnloadingEnabled"

javaOptions in run += "-XX:PermSize=512M"

javaOptions in run += "-XX:MaxPermSize=512M"

//addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.14")


