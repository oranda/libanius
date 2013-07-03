resolvers += Resolver.url("scalasbt releases", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots", 
		  "releases"  at "http://oss.sonatype.org/content/repositories/releases")


addSbtPlugin("org.scala-sbt" % "sbt-android-plugin" % "0.6.2")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.0.0")




