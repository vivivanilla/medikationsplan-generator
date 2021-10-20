val scala3Version = "3.0.2"
val http4sVersion = "0.23.4"
val tapirVersion = "0.19.0-M12"

lazy val root = project
  .in(file("."))
  .settings(
    name := "mpgen",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % tapirVersion,
      ("com.lihaoyi" %% "scalatags" % "0.8.2").cross(CrossVersion.for3Use2_13),
      "uk.org.okapibarcode" % "okapibarcode" % "0.3.0",
      "org.apache.xmlgraphics" % "batik-transcoder" % "1.14",
      "org.apache.xmlgraphics" % "fop-transcoder-allinone" % "2.6",
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-blaze-client" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion
    ),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
  )
