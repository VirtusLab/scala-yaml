lazy val scala3Version = "3.0.0"
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

lazy val munit: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(Deps.munit % Test),
  testFrameworks += new TestFramework("munit.Framework")
)

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
    name         := "scala-yaml-test-core",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(Deps.osLib % Test)
  )
  .settings(munit)
  .dependsOn(scalaYamlCore)

lazy val scalaYamlTestSuite = crossProject(JSPlatform, JVMPlatform)
  .in(file("tests/test-suite"))
  .settings(
    name         := "scala-yaml-test-suite",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(Deps.osLib % Test)
  )
  .settings(munit)
  .dependsOn(scalaYamlTestCore)

lazy val scalaYamlConfigTests = crossProject(JSPlatform, JVMPlatform)
  .in(file("tests/config-test"))
  .settings(
    name         := "scala-yaml-config-tests",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(Deps.osLib % Test)
  )
  .settings(munit)
  .dependsOn(scalaYamlTestCore)
