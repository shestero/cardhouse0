
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "cards1b"
  )

val http4sVersion = "0.23.13" 
libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion
)

val catsVersion = "2.5.4" // "3.4.8"
libraryDependencies += "org.typelevel" %% "cats-effect" % catsVersion
libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % "1.2.9"
libraryDependencies += "com.softwaremill.sttp.client3" %% "core" % "3.8.12"

ThisBuild / libraryDependencySchemes += "org.typelevel" %% "cats-effect" % "always"
