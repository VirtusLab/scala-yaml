package org.virtuslab.yaml

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.virtuslab.yaml.Tag
import org.virtuslab.yaml.internal.load.parse.Anchor
import org.virtuslab.yaml.internal.load.parse.EventKind
import org.virtuslab.yaml.internal.load.parse.EventKind._
import org.virtuslab.yaml.internal.load.parse.NodeEventMetadata
import org.virtuslab.yaml.internal.load.parse.ParserImpl
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.Tokenizer

trait TestRunner {
  def testOutYaml: Boolean
  def inYaml: String
  def outYaml: Option[String] = None
  def expectedEvents: String

  def run(): RunnerResult = {
    val reader = Tokenizer.make(inYaml)
    val parser = ParserImpl(reader)
    val acc    = new mutable.ArrayDeque[EventKind]()

    @tailrec
    def loop(): RunnerResult = {
      parser.getNextEvent() match {
        case Right(event) =>
          acc.append(event.kind)
          if (event.kind != EventKind.StreamEnd) loop()
          else RunnerResult(acc.toList, expectedEvents)
        case Left(error) =>
          RunnerResult(acc.toList, expectedEvents, error)
      }
    }

    val result = loop()

    if (testOutYaml) {
      // we don't compare output yaml if there is an error in event parsing
      if (
        outYaml.isDefined && (result match {
          case RunnerResult.Error(_, _) => false; case _ => true
        })
      ) {
        val roundtrip = inYaml.asNode match
          case Left(error) =>
            println(s"Error while parsing inYaml:\n$error")
            s"!!! ERROR: $error"
          case Right(value) => value.asYaml

        if (roundtrip != outYaml.map(_.appended('\n')).get)
          RunnerResult.InvalidOutYaml(result, roundtrip, outYaml.get)
        else result
      } else result

    } else result
  }

}

object TestRunnerUtils {

  implicit class OptionAnchorOps(anchor: Option[Anchor]) {
    def anchorAsString: String = anchor.map(a => s" &$a").getOrElse("")
  }
  implicit class OptionTagOps(tag: Option[Tag]) {
    def tagAsString: String = tag.map(a => s" <$a>").getOrElse("")
  }
  implicit class NodeEventMetadataOps(metadata: NodeEventMetadata) {
    def asString: String =
      List(
        metadata.anchor.anchorAsString,
        metadata.tag.tagAsString
      ).mkString
  }

  def convertEventToYamlTestSuiteFormat(events: Seq[EventKind]): String =
    events
      .map {
        case StreamStart             => "+STR"
        case StreamEnd               => "-STR"
        case DocumentStart(explicit) => if (explicit) "+DOC ---" else "+DOC"
        case DocumentEnd(explicit)   => if (explicit) "-DOC ..." else "-DOC"
        case SequenceStart(data)     => s"+SEQ${data.asString}"
        case SequenceEnd             => "-SEQ"
        case MappingStart(data)      => s"+MAP${data.asString}"
        case MappingEnd              => "-MAP"
        case Alias(alias)            => s"=ALI *$alias"
        case Scalar(value, style, data) =>
          val escapedValue = value.replace("\\", "\\\\").replace("\n", "\\n")
          style match {
            case ScalarStyle.Plain        => s"=VAL${data.asString} :$value"
            case ScalarStyle.DoubleQuoted => s"""=VAL${data.asString} "$value"""
            case ScalarStyle.SingleQuoted => s"=VAL${data.asString} '$escapedValue"
            case ScalarStyle.Folded       => s"=VAL${data.asString} >$value"
            case ScalarStyle.Literal      => s"=VAL${data.asString} |$value"
          }
      }
      .mkString("\n")

}

case class K8sYamlTestRunner(yamlPath: os.Path, libYaml: os.Path, testOutYaml: Boolean)
    extends TestRunner {
  override val inYaml = os.read(yamlPath)
  override val expectedEvents = os
    .proc(libYaml, yamlPath)
    .call(cwd = os.pwd)
    .out
    .text()
    .trim

  override def run(): RunnerResult = {
    println(yamlPath)
    super.run()
  }

}

case class YamlSuiteTestRunner(testYamlML: os.Path, testOutYaml: Boolean) extends TestRunner {
  private val testMl = TestMlEntry.from(testYamlML)

  override val outYaml        = testMl.outYaml
  override val inYaml         = testMl.inYaml
  override val expectedEvents = testMl.seqEvent

}
