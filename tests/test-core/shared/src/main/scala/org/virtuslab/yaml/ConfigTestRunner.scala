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

case class ConfigTestRunner(yamlPath: os.Path, libYaml: os.Path) extends TestRunner {

  def run(): RunnerResult = {
    println(yamlPath)
    val yaml   = os.read(yamlPath)
    val reader = Scanner(yaml)

    ParserImpl(reader).getEvents() match {
      case Right(events) => {
        val eventYamlTestSuite: String = TestRunnerUtils.convertEventToYamlTestSuiteFormat(events)
        val libYamlEvent               = eventFromLibYaml

        RunnerResult.Success(eventYamlTestSuite, libYamlEvent)
      }
      case Left(e) => RunnerResult.Exeception(e)
    }

  }

  private def eventFromLibYaml: String = {
    os
      .proc(libYaml, yamlPath)
      .call(cwd = os.pwd)
      .out
      .text()
      .trim
  }

}
