val scala3Version = "3.6.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "schrodinger",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.7",
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.6.0" % Test,
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    )
  )
