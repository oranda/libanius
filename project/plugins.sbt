resolvers += Resolver.url("scalasbt releases", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots", 
		  "releases"  at "http://oss.sonatype.org/content/repositories/releases")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.0.0")




