package org.virtuslab.yaml

import java.io.File

abstract class YamlRunnerSpec extends munit.FunSuite {

  def resourcePath: String
  def createTestRunner(yamlPath: os.Path): TestRunner

  val verbose                       = false
  val predicate: os.Path => Boolean = _ => true

  val yamlDirPath              = getClass.getResource(resourcePath)
  val yamlDir                  = new File(yamlDirPath.getPath)
  val yamlPaths: List[os.Path] = yamlDir.listFiles().map(os.Path(_)).filter(predicate).toList

  test("should parse yaml to event") {

    def loop(paths: List[os.Path], failsPath: List[os.Path]): List[os.Path] = {
      paths match {
        case path :: tail => {
          val testRunner = createTestRunner(path)
          if (verbose) {
            println(s"Running $path")
          }
          val runnerResult = testRunner.run()
          runnerResult match {
            case RunnerResult.Success(_) => loop(tail, failsPath)
            case RunnerResult.InvalidEvents(obtained, expected) =>
              println(s"Events differ - $path")
              if (verbose) {
                println(s"Obtained:\n$obtained")
                println(s"Expected:\n$expected")
              }
              loop(tail, path :: failsPath)
            case RunnerResult.Error(eventsUntilError, error) =>
              println(s"Error in test - $path")
              if (verbose) {
                println(s"Obtained:\n$eventsUntilError")
                println(s"Encountered error:\n$error")
              }
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
