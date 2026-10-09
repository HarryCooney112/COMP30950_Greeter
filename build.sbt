// *** DO NOT ADD DEPENDENCIES TO THIS FILE ****

val scala3Version = "3.3.8"

lazy val root = project
  .in(file("."))
  .settings(
    name := "greeter",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalatest" %% "scalatest-funsuite" % "3.2.18" % "test"
  )
  
