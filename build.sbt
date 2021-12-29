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

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

lazy val scalaYamlCore = crossProject(JSPlatform, JVMPlatform)
  .in(file("yaml"))
  .settings(
    name              := projectName,
    scalaVersion      := scala3Version,
    semanticdbEnabled := true,
    libraryDependencies ++= Seq(Deps.pprint % Test)
  )
  .settings(docsSettings)
  .settings(munit)

lazy val scalaYamlTestSuite = crossProject(JSPlatform, JVMPlatform)
  .in(file("tests/test-suite"))
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    name              := "testSuite",
    scalaVersion      := scala3Version,
    semanticdbEnabled := true
  )
  .settings(testSettings)
  .dependsOn(scalaYamlCore)
