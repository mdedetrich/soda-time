lazy val root = project.in(file(".")).
  aggregate(sodatimeJS, sodatimeJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val sodatime = crossProject.in(file(".")).
  settings(
    name := "soda-time",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.11.7"
  ).
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
    // Add JS-specific settings here
  )

lazy val sodatimeJVM = sodatime.jvm
lazy val sodatimeJS = sodatime.js
