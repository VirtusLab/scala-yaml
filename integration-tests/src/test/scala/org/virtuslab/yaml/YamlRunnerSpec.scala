package org.virtuslab.yaml

import java.io.File
import scala.util.matching.Regex

abstract class YamlRunnerSpec extends munit.FunSuite {

  def resourcePath: String
  def createTestRunner(yamlPath: os.Path): TestRunner

  def runMatching: Regex = ".*".r

  val testOutYaml: Boolean          = false
  val skipErrors: Boolean           = false
  val skipInvalidEvents: Boolean    = false
  val verbose: Boolean              = false
  val predicate: os.Path => Boolean = _ => true

  val yamlDirPath = getClass.getResource(resourcePath)
  if (yamlDirPath == null) {
    throw new IllegalArgumentException(
      s"Resource $resourcePath not found, run integrations-tests/downloadYamlConfigs.sh first!"
    )
  }
  val yamlDir                  = new File(yamlDirPath.getPath)
  val yamlPaths: List[os.Path] = yamlDir.listFiles().map(os.Path(_)).filter(predicate).toList

  case class Results(
      invalidEventsPaths: List[os.Path],
      errorsPaths: List[os.Path],
      successes: Int
  ) {
    def allFailures: List[os.Path] = invalidEventsPaths ++ errorsPaths

    def count = invalidEventsPaths.size + errorsPaths.size + successes
  }

  test("should parse yaml to event") {

    def loop(
        paths: List[os.Path],
        invalidEventsPaths: List[os.Path],
        errorsPaths: List[os.Path],
        successes: Int
    ): Results = {
      paths match {
        case path :: tail => {
          if (!runMatching.matches(path.toString)) {
            loop(tail, invalidEventsPaths, errorsPaths, successes)
          } else {
            val testRunner = createTestRunner(path)
            if (verbose) {
              println(s"Running $path")
            }
            val runnerResult = testRunner.run()
            runnerResult match {
              case RunnerResult.InvalidOutYaml(eventsResult, outYaml, expectedOutYaml) =>
                val escapedOutYaml = outYaml.replace("\\", "\\\\").replace("\n", "\\n")
                val escapedExpectedOutYaml =
                  expectedOutYaml.replace("\\", "\\\\").replace("\n", "\\n")
                println(s"Out yaml differ - $path")
                if (verbose && !skipInvalidEvents) {
                  println(s"Obtained:\n$outYaml")
                  println(s"Expected:\n$expectedOutYaml")
                  println("Escaped:")
                  println(s"Obtained:\n$escapedOutYaml")
                  println(s"Expected:\n$escapedExpectedOutYaml")
                }
                eventsResult match {
                  case RunnerResult.InvalidEvents(obtained, expected) =>
                    println(s"Events differ - $path")
                    if (verbose && !skipInvalidEvents) {
                      println(s"Obtained:\n$obtained")
                      println(s"Expected:\n$expected")
                    }
                  case _ =>
                }
                loop(tail, path :: invalidEventsPaths, errorsPaths, successes)
              case RunnerResult.Success(_) =>
                loop(tail, invalidEventsPaths, errorsPaths, successes + 1)
              case RunnerResult.InvalidEvents(obtained, expected) =>
                println(s"Events differ - $path")
                if (verbose && !skipInvalidEvents) {
                  println(s"Obtained:\n$obtained")
                  println(s"Expected:\n$expected")
                }
                loop(tail, path :: invalidEventsPaths, errorsPaths, successes)
              case RunnerResult.Error(eventsUntilError, error) =>
                println(s"Error in test - $path")
                if (verbose && !skipErrors) {
                  println(s"Obtained:\n$eventsUntilError")
                  println(s"Encountered error:\n$error")
                }
                loop(tail, invalidEventsPaths, path :: errorsPaths, successes)
            }
          }
        }
        case Nil => Results(invalidEventsPaths, errorsPaths, successes)
      }
    }

    val results = loop(yamlPaths, Nil, Nil, 0)

    // I need to refactor this summary so that it takes errors and invalid events into account
    // if one of them are to be skipped, stats should disregard them (total should be smaller, failed should be of non-skipped type)
    val total = results.count

    val all =
      if (skipErrors && skipInvalidEvents) results.count - results.allFailures.size
      else if (skipErrors) results.count - results.errorsPaths.size
      else if (skipInvalidEvents) results.count - results.invalidEventsPaths.size
      else results.count

    val invalidEvents = results.invalidEventsPaths.size
    val errors        = results.errorsPaths.size

    val skipped = if (skipErrors) invalidEvents else if (skipInvalidEvents) errors else 0

    val failed =
      if (skipErrors && skipInvalidEvents) 0
      else if (skipErrors) invalidEvents
      else if (skipInvalidEvents) errors
      else invalidEvents + errors

    val passed = all - failed

    val summary = s"""|
                      |SUMMARY
                      |
                      |Total tests: $total 
                      |Skipped: $skipped (skipped errors: $skipErrors (encountered $errors), skipped invalid events: $skipInvalidEvents (encountered $invalidEvents))
                      |Summarised tests: $all
                      |
                      |Passed: $passed/$all ${"%.2f".format(100 * passed.toFloat / all.toFloat)}%
                      |Failed: $failed/$all ${"%.2f".format(100 * failed.toFloat / all.toFloat)}%
                      |""".stripMargin
    println(summary)

    assert(results.allFailures.isEmpty)
  }

}
