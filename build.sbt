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

lazy val root = project
  .in(file("."))
  .settings(
    name              := projectName,
    scalaVersion      := scala3Version,
    semanticdbVersion := scalafixSemanticdb.revision,
    semanticdbEnabled := true
  )
  .settings(munit)
