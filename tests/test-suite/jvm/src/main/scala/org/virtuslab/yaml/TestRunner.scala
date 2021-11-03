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
    val acc    = new mutable.ArrayDeque[Event]()

    @tailrec
    def loop(): RunnerResult = {
      parser.getNextEvent() match
        case Right(event) =>
          acc.append(event)
          if event != Event.StreamEnd then loop()
          else RunnerResult(acc.toList, expectedEvents)
        case Left(error) =>
          RunnerResult(acc.toList, expectedEvents, error)
    }
    loop()
  end run

end TestRunner

object TestRunnerUtils:

  extension (anchor: Option[String])
    def asString: String = anchor.map(anchor => s" &$anchor").getOrElse("")

  def convertEventToYamlTestSuiteFormat(events: Seq[Event]): String =
    events
      .map {
        case _: Event.StreamStart                          => "+STR"
        case _: Event.StreamEnd                            => "-STR"
        case Event.DocumentStart(_, explicit)              => if (explicit) "+DOC ---" else "+DOC"
        case Event.DocumentEnd(_, explicit)                => if (explicit) "-DOC ..." else "-DOC"
        case s: Event.SequenceStart                        => s"+SEQ${s.anchor.asString}"
        case _: Event.SequenceEnd                          => "-SEQ"
        case m: Event.MappingStart                         => s"+MAP${m.anchor.asString}"
        case m: Event.FlowMappingStart                     => s"+MAP${m.anchor.asString}"
        case _: Event.MappingEnd | _: Event.FlowMappingEnd => "-MAP"
        case Event.Alias(id, _)                            => s"=ALI *$id"
        case Event.Scalar(value, style, _, anchor) =>
          style match {
            case ScalarStyle.Plain        => s"=VAL${anchor.asString} :$value"
            case ScalarStyle.DoubleQuoted => s"""=VAL${anchor.asString} "$value"""
            case ScalarStyle.SingleQuoted => s"=VAL${anchor.asString} '$value"
            case ScalarStyle.Folded       => s"=VAL${anchor.asString} >$value"
            case ScalarStyle.Literal      => s"=VAL${anchor.asString} |$value"
          }
      }
      .mkString("\n")

end TestRunnerUtils

case class K8sYamlTestRunner(yamlPath: os.Path, libYaml: os.Path) extends TestRunner:
  override val inYaml = os.read(yamlPath)
  override val expectedEvents = os
    .proc(libYaml, yamlPath)
    .call(cwd = os.pwd)
    .out
    .text()
    .trim

  override def run(): RunnerResult =
    println(yamlPath)
    super.run()

end K8sYamlTestRunner

case class YamlSuiteTestRunner(testYamlML: os.Path) extends TestRunner:
  private val testMl = TestMlEntry.from(testYamlML)

  override val inYaml         = testMl.inYaml
  override val expectedEvents = testMl.seqEvent

end YamlSuiteTestRunner
