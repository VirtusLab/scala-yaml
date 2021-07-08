val scala3Version = "3.0.0"
val projectName = "scala-yaml"
val projectVersion = "0.0.1"

lazy val utest: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(Deps.munit % Test, Deps.expecty % Test),
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val root = project
  .in(file("."))
  .settings(
    name := projectName,
    version := projectVersion,
    scalaVersion := scala3Version,
    scalacOptions += "-Ywarn-unused-import",
    semanticdbVersion := scalafixSemanticdb.revision,
    semanticdbEnabled := true
  )
  .settings(utest)
