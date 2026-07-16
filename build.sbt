import scala.collection.Seq

lazy val scala213 = "2.13.18"
lazy val scala3 = "3.3.8"
lazy val supportedScalaVersions = List(scala213, scala3)

lazy val scalaTestVersion = "3.2.20"
lazy val scalaCheckVersion = "1.19.0"
lazy val scalaTestPlusScalaCheckVersion = "3.2.20.0"

lazy val root = project
  .in(file("."))
  .aggregate(scaletta.jvm, scaletta.js)
  .settings(
    name := "scaletta",
    publish := {},
    publishLocal := {},
    crossScalaVersions := Nil,
  )

lazy val scaletta = crossProject(JVMPlatform, JSPlatform)
  .in(file("."))
  .settings(
    name := "scaletta",
    version := "0.1-SNAPSHOT",
    scalaVersion := scala213,
    crossScalaVersions := supportedScalaVersions,
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
      "org.scalacheck" %% "scalacheck" % scalaCheckVersion % Test,
      "org.scalatestplus" %% "scalacheck-1-19" % scalaTestPlusScalaCheckVersion % Test,
    )

  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % scalaTestVersion % Test,
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % Test,
      "org.scalatestplus" %%% "scalacheck-1-19" % scalaTestPlusScalaCheckVersion % Test,
    )
  )
