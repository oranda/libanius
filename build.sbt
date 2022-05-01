import sbt._

organization := "com.github.oranda"
name := "libanius"

version := "0.9.8.7.3"

scalaVersion := "2.12.6"

homepage := Some(url("http://github.com/oranda/libanius"))

licenses += ("GNU Affero General Public License", url("https://www.gnu.org/licenses/agpl-3.0.en.html"))

scmInfo := Some(ScmInfo(
  url("https://github.com/oranda/libanius"),
  "scm:git:git@github.com/oranda/libanius.git",
  Some("scm:git:git@github.com/oranda/libanius.git")))

developers := List(
  Developer(
    id = "oranda",
    name = "James McCabe",
    email = "jjtmccabe@gmail.com",
    url = url("https://github.com/oranda")
  )
)

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

scalacOptions ++= Seq("-unchecked", "-deprecation")

resolvers ++= Seq("mvnrepository" at "https://mvnrepository.com/artifact/")

val typesafeConfigVersion = "1.4.0"

libraryDependencies ++= Seq("com.typesafe" % "config" % typesafeConfigVersion,
  "org.specs2" %% "specs2-core" % "4.2.0" % "test",
  "org.specs2" %% "specs2-junit" % "4.2.0" % "test",
  "org.scalaz" %% "scalaz-core" % "7.2.25",
  "org.apache.httpcomponents" % "httpclient" % "4.1.2",
  "com.typesafe.play" %% "play-json" % "2.6.7",
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
)

parallelExecution in Test := true

// an unmanaged dependency is no longer used, but these settings are retained in case it is needed
assemblyJarName in assembly := s"${name.value}-${version.value}-fat.jar"
assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false)

// Exclude the config jar from the fat jar. Ideally the config jar would be excluded
// from the classpath using Provided scope, but this spoils the run task, so instead:
assemblyExcludedJars in assembly := {
  val cp = (fullClasspath in assembly).value
  cp filter {_.data.getName == s"config-$typesafeConfigVersion.jar"}
}

test in assembly := {}

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"

exportJars := false

fork := false

javaOptions in run += "-XX:+UseConcMarkSweepGC"

javaOptions in run += "-XX:+CMSClassUnloadingEnabled"

javaOptions in run += "-XX:PermSize=512M"

javaOptions in run += "-XX:MaxPermSize=512M"


import sbt._

addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")


