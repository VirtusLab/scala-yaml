package org.virtuslab.yaml

import org.virtuslab.yaml
import org.virtuslab.yaml.TestMlEntry
import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.parse.Event
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle._
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.Token._
import org.virtuslab.yaml.internal.load.parse.ParserImpl
import org.virtuslab.yaml.internal.load.reader.Scanner
import os._

import java.io.File
import scala.util.Success
import scala.util.Failure
import scala.util.Try

class TestSuiteSpec extends munit.FunSuite {

  val yamlDirPath              = getClass.getResource("/yaml")
  val yamlDir                  = new File(yamlDirPath.getPath)
  val yamlPaths: List[os.Path] = yamlDir.listFiles().map(Path(_)).toList

  test("should parse only one yaml from test suite path") {

    val path: os.Path = os.Path(
      "/Users/lwronski/projects/scala-yaml/tests/test-suite/jvm/target/scala-3.0.0/test-classes/yaml/K527.tml"
    )
    val testMl = TestMlEntry.from(os.read(path))

    val reader = Scanner(testMl.inYaml)
    val events = ParserImpl.getEvents(reader) match {
      case Right(v) => v
      case Left(exception) => {
        println(exception)
        Nil
      }
    }

    val eventYamlTestSuite: String = ConfigUtils.convertEventToYamlTestSuiteFormat(events)

    val expectedEvent: String = testMl.testEvent

    assertEquals(eventYamlTestSuite, expectedEvent)
  }

  test("should official test suite yaml to event") {

    def loop(paths: List[os.Path], failsPath: List[os.Path]): List[os.Path] = {
      paths match {
        case path :: tail => {
          val testMl = TestMlEntry.from(os.read(path))

          val reader = Scanner(testMl.inYaml)
          println(path)
          val events =
            Try(ParserImpl.getEvents(reader).getOrElse(sys.error("Parsing yaml to event"))) match {
              case Success(v) => v
              case Failure(e) => Nil
            }
          val eventYamlTestSuite: String = ConfigUtils.convertEventToYamlTestSuiteFormat(events)

          val expectedEvent: String = testMl.testEvent

          if (eventYamlTestSuite == expectedEvent) {
            loop(tail, failsPath)
          } else {
            println(s"Failed path - $path")
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
