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

    val failsPath = loop(yamlPaths, Nil)

    val all    = yamlPaths.size
    val failed = failsPath.size
    val passed = (yamlPaths.size - failsPath.size)

    val summary = s"""|
                      |SUMMARY
                      |
                      |Passed: $passed/$all ${"%.2f".format(100 * passed.toFloat / all.toFloat)}%
                      |Failed: $failed/$all ${"%.2f".format(100 * failed.toFloat / all.toFloat)}%
                      |""".stripMargin
    println(summary)

    assert(failsPath.isEmpty)
  }

}
