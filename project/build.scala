import sbt._

import Keys._
import AndroidKeys._

object General {

  val settings = Defaults.defaultSettings ++ Seq (
    name := "Libanius",
    version := "0.74",
    versionCode := 74,
    scalaVersion := "2.10.2",
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
  val proguardOptions = Seq(
    proguardOption in Android := "-keep class scala.collection.SeqLike {\n    public protected *;\n}"
  )

  lazy val fullAndroidSettings =
    General.settings ++
    AndroidProject.androidSettings ++
    TypedResources.settings ++
    proguardSettings ++
    proguardOptions ++
    AndroidManifestGenerator.settings ++
    AndroidMarketPublish.settings ++ Seq (
      keyalias in Android := "alias_name",
      libraryDependencies ++= Seq("org.specs2" %% "specs2" % "2.0" % "test")
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
