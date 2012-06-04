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
    version := "0.4",
    versionCode := 0,
    scalaVersion := "2.9.1",
    platformName in Android := "android-8",
    scalacOptions += "-deprecation"
    //resolvers += robospecsReleases,
    //libraryDependencies += robospecs
  )


  val proguardSettings = Seq (
    useProguard in Android := false  // enable for deploy to device
  )

  lazy val fullAndroidSettings =
    General.settings ++
    AndroidProject.androidSettings ++
    TypedResources.settings ++
    proguardSettings ++
    AndroidManifestGenerator.settings ++
    AndroidMarketPublish.settings ++ Seq (
      keyalias in Android := "change-me",
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
    )
  ) dependsOn main



}
