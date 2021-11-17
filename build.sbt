// scalafmt: { align.preset = most, danglingParentheses.preset = false }

val V = new {
  val Scala = "3.1.0"

  val Zio       = "2.0.0-M5"
  val ZioConfig = "1.0.10"
  val ZioHttp   = "1.0.0.0-RC17"
}

inThisBuild(
  List(
    name             := "Ziokoban",
    scalaVersion     := V.Scala,
    version          := "0.1.0-SNAPSHOT",
    organization     := "nl.itvanced.ziokoban",
    organizationName := "itvanced",
    maintainer       := "rob.vermazeren@itvanced.nl"
  )
)

ThisBuild / scalacOptions ++= List(
  "-encoding",
  "utf-8",
  "-feature",
  "-unchecked",
  "-deprecation",
//  "-Xlint:-unused,_",
//  "-Xcheckinit",
  "-Xfatal-warnings",
//  "-Ywarn-numeric-widen",
//  "-Ywarn-unused:imports"
)

lazy val root = project
  .in(file("."))
  .settings(
    name         := "ziokoban",
    scalaVersion := V.Scala
  )
  .aggregate(
    core,
  )

lazy val core = project
  .in(file("modules/core"))
  .enablePlugins(JvmPlugin, JavaAppPackaging)
  .settings(
    scalaVersion := V.Scala,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % V.Zio % "test",
      "dev.zio" %% "zio-test"     % V.Zio % "test",
      "dev.zio" %% "zio-test-sbt" % V.Zio % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
