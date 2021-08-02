package org.virtuslab.yaml

import org.virtuslab.yaml
import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.parse.{Event, ParserImpl}
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle._
import org.virtuslab.yaml.internal.load.reader.token.Token._
import org.virtuslab.yaml.utils.{ConfigUtils, TestMlEntry}

import java.io.File
import scala.util.{Failure, Success, Try}

class TestSuiteSpec extends munit.FunSuite {

  val yamlDirPath              = getClass.getResource("/yaml/test-suite")
  val yamlDir                  = new File(yamlDirPath.getPath)
  val yamlPaths: List[os.Path] = yamlDir.listFiles().map(os.Path(_)).toList

  test("should parse only one yaml from test suite path".ignore) {

    val path: os.Path = ???
    val testMl        = TestMlEntry.from(os.read(path))

    val reader = Scanner(testMl.inYaml)
    val events = ParserImpl.getEvents(reader) match {
      case Right(v) => v
      case Left(exception) => {
        println(exception)
        Nil
      }
    }

    val eventYamlTestSuite: String = ConfigUtils.convertEventToYamlTestSuiteFormat(events)

    val expectedEvent: String = testMl.seqEvent

    assertEquals(eventYamlTestSuite, expectedEvent)
  }

  test("should official test suite yaml to event") {

    def loop(paths: List[os.Path], failsPath: List[os.Path]): List[os.Path] = {
      paths match {
        case path :: tail => {
          val testMl = TestMlEntry.from(os.read(path))

          val reader = Scanner(testMl.inYaml)
          val events =
            Try(ParserImpl.getEvents(reader).getOrElse(sys.error("Parsing yaml to event"))) match {
              case Success(v) => v
              case Failure(e) => Nil
            }
          val eventYamlTestSuite: String = ConfigUtils.convertEventToYamlTestSuiteFormat(events)

          val expectedEvent: String = testMl.seqEvent

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
