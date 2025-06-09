ThisBuild / scalaVersion     := "2.13.14"
ThisBuild / version          := "0.4.0"
ThisBuild / organization     := "com.github.chadxbaker52"

val chiselVersion = "3.6.1"

lazy val root = (project in file("."))
  .settings(
    name := "chisel-sudoku-solver",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "0.6.2" % "test"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-P:chiselplugin:genBundleElements",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
    Test / fork := true,
    Test / javaOptions ++= Seq(
      "-Xms2G",  // initial heap
      "-Xmx4G"   // max heap
    )
  )

libraryDependencies += "org.scalatestplus" %% "junit-4-13" % "3.2.15.0" % "test"
