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
import scala.annotation.tailrec
import scala.collection.mutable

trait TestRunner():
  def inYaml: String
  def expectedEvents: String

  def run(): RunnerResult =
    val reader = Scanner(inYaml)
    val parser = ParserImpl(reader)
    val acc    = new mutable.ArrayDeque[Event](50)

    @tailrec
    def loop(): RunnerResult = {
      parser.getNextEvent() match
        case Right(event) =>
          acc.append(event)
          if event != Event.StreamEnd then loop()
          else
            val events   = TestRunnerUtils.convertEventToYamlTestSuiteFormat(acc.toSeq)
            val expected = expectedEvents
            RunnerResult.Success(events, expected)
        case Left(e) =>
          val events = TestRunnerUtils.convertEventToYamlTestSuiteFormat(acc.toSeq)
          RunnerResult.Exception(events, e)
    }
    loop()
  end run

end TestRunner
