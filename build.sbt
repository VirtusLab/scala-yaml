import BuildHelper._

def scala3Version        = "3.1.2"
def projectName          = "scala-yaml"
def localSnapshotVersion = "0.0.5-SNAPSHOT"
def isCI                 = System.getenv("CI") != null

inThisBuild(
  List(
    organization := "org.virtuslab",
    crossScalaVersions := Seq("2.13.8", scala3Version),
    scalaVersion := scala3Version,
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
        "kpodsiadlo@virtuslab.com",
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
    name              := projectName,
    semanticdbEnabled := true,
    libraryDependencies ++= Seq(Deps.pprint % Test),

    // see https://github.com/scala-native/scala-native/blob/master/docs/changelog/0.4.3-RC1.md#cannot-create-documentation-using-scaladoc-in-scala-native-sbt-project
    Compile / doc / scalacOptions ~= { options =>
      options.filterNot(_.startsWith("-Xplugin"))
    }
  )
  .jsSettings(
    libraryDependencies ++= List(
      "org.scalameta" %%% "munit"                  % "0.7.29"       % Test,
      ("org.scala-js"  %% "scalajs-test-interface" % scalaJSVersion % Test)
        .cross(CrossVersion.for3Use2_13),
      ("org.scala-js" %% "scalajs-junit-test-runtime" % scalaJSVersion % Test)
        .cross(CrossVersion.for3Use2_13)
    )
  )
  .nativeSettings(
    // skip native tests for now since upstream changes in munit are required
    Test / compile / skip := true,
    Test / test / skip    := true,

    // set dummy directory with tests to avoid unnecessary errors
    Test / unmanagedSourceDirectories := Nil

    // libraryDependencies ++= List(
    // ("org.scalameta" %% "munit"  % "0.7.29"  % Test).cross(CrossVersion.for3Use2_13),
    // ("org.scala-native" %%% "test-interface" % nativeVersion  % Test).cross(CrossVersion.for3Use2_13),
    // ),
  )
  .settings(docsSettings)
  .jvmSettings(
    libraryDependencies ++= List(
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )

lazy val integration = project
  .in(file("integration-tests"))
  .dependsOn(core.jvm)
  .settings(
    name              := "integration",
    moduleName        := "integration",
    semanticdbEnabled := true,
    publish / skip    := true,
    libraryDependencies ++= List(
      "org.scalameta" %% "munit"  % "0.7.29",
      "com.lihaoyi"   %% "os-lib" % "0.8.1",
      "com.lihaoyi"   %% "pprint" % "0.7.3"
    )
  )
  .settings(docsSettings)
