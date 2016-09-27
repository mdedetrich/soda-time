name := "Soda Time"

val scala211Version = "2.11.8"
val scala210Version = "2.10.6"

scalaVersion in ThisBuild := scala211Version
crossScalaVersions in ThisBuild := Seq(scala211Version, scala210Version)

lazy val root = project
  .in(file("."))
  .aggregate(sodatimeJS, sodatimeJVM)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val sodatime = crossProject
  .in(file("."))
  .settings(
    name := "soda-time",
    version := "0.0.1-SNAPSHOT",
    organization := "org.mdedetrich",
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    publishArtifact in Test := false,
    pomIncludeRepository := { _ =>
      false
    },
    pomExtra := <url>https://github.com/mdedetrich/soda-time</url>
      <licenses>
        <license>
          <name>BSD-style</name>
          <url>http://www.opensource.org/licenses/bsd-license.php</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:mdedetrich/soda-time.git</url>
        <connection>scm:git:git@github.com:mdedetrich/soda-time.git</connection>
      </scm>
      <developers>
        <developer>
          <id>mdedetrich</id>
          <name>Matthew de Detrich</name>
          <email>mdedetrich@gmail.com</email>
        </developer>
      </developers>
  )
  .jvmSettings(
    // Add JVM-specific settings here
  )
  .jsSettings(
    // Add JS-specific settings here
  )

lazy val sodatimeJVM = sodatime.jvm
lazy val sodatimeJS = sodatime.js
