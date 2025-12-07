import scala.collection.Seq

lazy val scala213 = "2.13.17"
lazy val scala3 = "3.3.7"
lazy val supportedScalaVersions = List(scala213, scala3)

lazy val scalaTestVersion = "3.2.19"
lazy val scalaCheckVersion = "1.18.0"
lazy val scalaTestPlusScalaCheckVersion = "3.2.19.0"

lazy val root = project
  .in(file("."))
  .aggregate(scaletta.js, scaletta.jvm)
  .settings(
    publish := {},
    publishLocal := {},
    crossScalaVersions := Nil,
  )

lazy val scaletta = crossProject(JSPlatform, JVMPlatform)
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
      "org.scalatestplus" %% "scalacheck-1-18" % scalaTestPlusScalaCheckVersion % Test,
    )

  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % scalaTestVersion % Test,
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % Test,
      "org.scalatestplus" %%% "scalacheck-1-18" % scalaTestPlusScalaCheckVersion % Test,
    )
  )
