import sbt._

organization := "com.github.oranda"

name := "libanius"

version := "0.9.9.2"

scalaVersion := "3.1.2"

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

import xerial.sbt.Sonatype._

publishMavenStyle := true

sonatypeProfileName    := "com.github.oranda"
sonatypeProjectHosting := Some(GitHubHosting(user = "oranda", repository = "libanius", email = "jjtmccabe@gmail.com"))

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

scalacOptions ++= Seq("-unchecked", "-deprecation", "-new-syntax", "-rewrite", "-feature")

scalafmtOnCompile := false

resolvers ++= Seq("Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
  "mvnrepository" at "https://mvnrepository.com/artifact/"
)

val typesafeConfigVersion = "1.4.0"

ThisBuild / scalafixDependencies += "com.nequissimus" %% "sort-imports" % "0.6.1"

libraryDependencies ++= Seq("com.typesafe" % "config" % typesafeConfigVersion,
  "org.specs2" % "specs2-core_3" % "4.15.0" % "test",
  "org.specs2" % "specs2-junit_3" % "4.15.0" % "test",
  "org.scalaz" % "scalaz-core_3" % "7.3.6",
  "org.apache.httpcomponents" % "httpclient" % "4.1.2",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.13.18",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.13.18" % "compile-internal"
)

Test / parallelExecution := true

// an unmanaged dependency is no longer used, but these settings are retained in case it is needed
assembly / assemblyJarName := s"${name.value}-${version.value}-fat.jar"
assembly / assemblyOption := (assembly / assemblyOption).value.copy(includeScala = false)

// Exclude the config jar from the fat jar. Ideally the config jar would be excluded
// from the classpath using Provided scope, but this spoils the run task, so instead:
assembly / assemblyExcludedJars := {
  val cp = (assembly / fullClasspath).value
  cp filter {_.data.getName == s"config-$typesafeConfigVersion.jar"}
}

assembly / test := {}

console / initialCommands := "import scalaz._, Scalaz._"

exportJars := false

fork := false

run / javaOptions += "-XX:+UseConcMarkSweepGC"

run / javaOptions += "-XX:+CMSClassUnloadingEnabled"

run / javaOptions += "-XX:PermSize=512M"

run / javaOptions += "-XX:MaxPermSize=512M"
