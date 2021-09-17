package org.virtuslab.yaml

import org.virtuslab.yaml.internal.load.parse.ParserImpl

import scala.util.{Failure, Success, Try}

import org.virtuslab.yaml
import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.parse.{Event, ParserImpl}
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle._
import org.virtuslab.yaml.internal.load.reader.token.Token._

case class TestSuiteRunner(testYamlML: os.Path) extends TestRunner {

  override def run(): RunnerResult =
    val testMl = TestMlEntry.from(os.read(testYamlML))
    val reader = Scanner(testMl.inYaml)

    ParserImpl(reader).getEvents() match
      case Right(events) =>
        val eventYamlTestSuite  = TestRunnerUtils.convertEventToYamlTestSuiteFormat(events)
        val testSuiteYamlEvents = testMl.seqEvent

        RunnerResult.Success(eventYamlTestSuite, testSuiteYamlEvents)
      case Left(e) => RunnerResult.Exeception(e)

}
