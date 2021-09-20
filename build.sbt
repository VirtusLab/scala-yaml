import BuildHelper._

lazy val scala3Version = "3.0.2"
lazy val projectName   = "scala-yaml"

inThisBuild(
  List(
    organization := "org.virtuslab",
    homepage     := Some(url("https://github.com/VirtusLab/scala-yaml")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "lwronski",
        "Łukasz Wroński",
        "lukaszwronski97@gmail.com",
        url("https://github.com/lwronski")
      ),
      Developer(
        "kpodsiad",
        "Kamil Podsiadło",
        "kpodsiadlo@virtuslab.com",
        url("https://github.com/kpodsiad")
      )
    )
  )
)

lazy val scalaYamlCore = crossProject(JSPlatform, JVMPlatform)
  .in(file("yaml"))
  .settings(
    name              := projectName,
    scalaVersion      := scala3Version,
    semanticdbVersion := scalafixSemanticdb.revision,
    semanticdbEnabled := true
  )
  .settings(docsSettings)
  .settings(munit)

lazy val scalaYamlTestCore = crossProject(JSPlatform, JVMPlatform)
  .in(file("tests/test-core"))
  .settings(
    name         := "testCore",
    scalaVersion := scala3Version
  )
  .settings(testSettings)
  .dependsOn(scalaYamlCore)

lazy val scalaYamlTestSuite = crossProject(JSPlatform, JVMPlatform)
  .in(file("tests/test-suite"))
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    name         := "testSuite",
    scalaVersion := scala3Version
  )
  .settings(testSettings)
  .dependsOn(scalaYamlTestCore)

Global / onLoad ~= { old =>
  if (!scala.util.Properties.isWin) {
    import java.nio.file._
    val prePush = Paths.get(".git", "hooks", "pre-push")
    Files.createDirectories(prePush.getParent)
    Files.write(
      prePush,
      """#!/bin/sh
        |set -eux
        |bin/scalafmt --diff --diff-branch main
        |git diff --exit-code
        |""".stripMargin.getBytes()
    )
    prePush.toFile.setExecutable(true)
  }
  old
}
