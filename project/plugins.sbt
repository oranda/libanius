resolvers += Resolver.url("scalasbt releases", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots", 
                  "releases"  at "http://oss.sonatype.org/content/repositories/releases",
                  "scct-github-repository" at "http://mtkopone.github.com/scct/maven-repo",
                  Classpaths.typesafeResolver
                 )

addSbtPlugin("reaktor" % "sbt-scct" % "0.2-SNAPSHOT")





