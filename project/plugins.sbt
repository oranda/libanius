resolvers += Resolver.url("scalasbt releases", new URL("https://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers ++= Seq("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "https://oss.sonatype.org/content/repositories/releases",
                  "scct-github-repository" at "https://mtkopone.github.com/scct/maven-repo",
  Classpaths.typesafeReleases
                 )

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.3")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.0")





