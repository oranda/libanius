import sbt._

import Keys._
import AndroidKeys._

object General {

  /* 
    Robolectric stuff - not currently used

    val robospecs = "com.github.jbrechtel" %% "robospecs" % "0.2" % "test"
    val robospecsSnapshots  = "snapshots" at "http://jbrechtel.github.com/repo/snapshots" 
    val robospecsReleases  = "releases" at "http://jbrechtel.github.com/repo/releases" 
    val snapshots = "snapshots" at "http://scala-tools.org/repo-snapshots"
    val releases  = "releases" at "http://scala-tools.org/repo-releases"
  */

  val settings = Defaults.defaultSettings ++ Seq (
    name := "Libanius",
    version := "0.6",
    versionCode := 58,
    scalaVersion := "2.9.1",
    platformName in Android := "android-14",  // formerly android-8
    scalacOptions += "-deprecation",
    parallelExecution in Test := false,
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    libraryDependencies ++= Seq("com.typesafe.config" % "config" % "0.3.0"),
    unmanagedClasspath in Runtime <+= (baseDirectory) map { bd => Attributed.blank(bd / "config") }
  )

  val proguardSettings = Seq (
    useProguard in Android := true  // enable for deploy to device
  )

  lazy val fullAndroidSettings =
    General.settings ++
    AndroidProject.androidSettings ++
    TypedResources.settings ++
    proguardSettings ++
    AndroidManifestGenerator.settings ++
    AndroidMarketPublish.settings ++ Seq (
      keyalias in Android := "alias_name",
      libraryDependencies ++= Seq("org.specs2" %% "specs2" % "1.9" % "test")
    )
}

object AndroidBuild extends Build {
  lazy val main = Project (
    "Libanius",
    file("."),
    settings = General.fullAndroidSettings
  )

  lazy val tests = Project (
    "tests",
    file("tests"),
    settings = General.settings ++
               AndroidTest.settings ++
               General.proguardSettings ++ Seq (
      name := "LibaniusTests"    
      //resolvers += robospecsReleases,
      //libraryDependencies += robospecs
    )
  ) dependsOn main

}
