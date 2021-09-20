package org.virtuslab.yaml

import java.io.File

abstract class YamlRunnerSpec extends munit.FunSuite {

  def resourcePath: String
  def createTestRunner(yamlPath: os.Path): TestRunner

  val yamlDirPath              = getClass.getResource(resourcePath)
  val yamlDir                  = new File(yamlDirPath.getPath)
  val yamlPaths: List[os.Path] = yamlDir.listFiles().map(os.Path(_)).toList

  test("should parse yaml to event") {

    def loop(paths: List[os.Path], failsPath: List[os.Path]): List[os.Path] = {
      paths match {
        case path :: tail => {

          println(s"Test - $path")
          val runnerResult = createTestRunner(path).run()

          if (runnerResult.isValid) {
            loop(tail, failsPath)
          } else {
            println(s"Failed test - $path")
            loop(tail, path :: failsPath)
          }
        }
        case Nil => failsPath
      }
    }

    val failsPath         = loop(yamlPaths, Nil)
    val successParsedYaml = (yamlPaths.size - failsPath.size).toDouble

    println(
      s"$successParsedYaml - ${yamlPaths.size} - ${"%.2f"
        .format(successParsedYaml / yamlPaths.size * 100)}%"
    )

    assert(failsPath.isEmpty)
  }

}
