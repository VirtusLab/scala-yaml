import sbt._
import sbt.Keys._

object BuildHelper {
  lazy val docsSettings = Seq(
    Compile / doc / scalacOptions ++= Seq(
      "-project",
      "Scala-yaml",
      "-siteroot",
      "docs",
      "-project-version",
      version.value,
      "-project-logo",
      "docs/logo.svg",
      "-social-links:" +
        "github::https://github.com/VirtusLab/scala-yaml," +
        "twitter::https://twitter.com/VirtusLab",
      "-project-footer",
      s"Copyright (c) 2021, VirtusLab",
      "-source-links:github://VirtusLab/scala-yaml",
      "-revision",
      "master"
    ),
    Compile / doc := {
      val out = (Compile / doc).value
      IO.copyDirectory((Compile / doc / target).value, file("generated-docs"))
      out
    }
  )
}
