import BuildHelper._

def scala3Version        = "3.3.7"
def scala2Version        = "2.13.17"
def projectName          = "scala-yaml"
def localSnapshotVersion = "0.2.0-SNAPSHOT"
def isCI                 = System.getenv("CI") != null

enablePlugins(NoPublishPlugin)

inThisBuild(
  List(
    organization       := "org.virtuslab",
    crossScalaVersions := Seq(scala2Version, scala3Version),
    scalaVersion       := scala3Version,
    version ~= { dynVer =>
      if (isCI) dynVer
      else localSnapshotVersion // only for local publishing
    },
    homepage := Some(url("https://github.com/VirtusLab/scala-yaml")),
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
        "kamilpodsiadlo44@gmail.com",
        url("https://github.com/kpodsiad")
      )
    )
  )
)

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .withoutSuffixFor(JVMPlatform)
  .settings(
    name := projectName,
    libraryDependencies ++= List(
      Deps.munit  % Test,
      Deps.pprint % Test
    ),
    // add pprint conditionally only on local machines
    libraryDependencies ++= {
      if (isCI) Nil
      else List(Deps.pprint)
    }
  )
  .settings(docsSettings)

lazy val integration = project
  .in(file("integration-tests"))
  .dependsOn(core.jvm)
  .settings(
    name       := "integration",
    moduleName := "integration",
    libraryDependencies ++= List(
      Deps.munit,
      Deps.osLib,
      Deps.pprint
    ),
    Compile / doc / sources := Seq.empty
  )
  .enablePlugins(NoPublishPlugin)
