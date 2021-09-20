package org.virtuslab.yaml

import org.virtuslab.yaml.internal.load.parse.Event
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

object TestRunnerUtils:

  def convertEventToYamlTestSuiteFormat(event: Seq[Event]): String =
    event
      .map(event =>
        event match
          case _: Event.StreamStart             => "+STR"
          case _: Event.StreamEnd               => "-STR"
          case Event.DocumentStart(_, explicit) => if (explicit) "+DOC ---" else "+DOC"
          case Event.DocumentEnd(_, explicit)   => if (explicit) "-DOC ---" else "-DOC"
          case _: Event.SequenceStart           => "+SEQ"
          case _: Event.SequenceEnd             => "-SEQ"
          case _: Event.MappingStart | _: Event.FlowMappingStart => "+MAP"
          case _: Event.MappingEnd | _: Event.FlowMappingEnd     => "-MAP"
          case Event.Scalar(value, style, _) =>
            style match {
              case ScalarStyle.Plain        => s"=VAL :$value"
              case ScalarStyle.DoubleQuoted => s"""=VAL "$value"""
              case ScalarStyle.SingleQuoted => s"=VAL '$value"
              case ScalarStyle.Folded       => s"=VAL >$value"
              case ScalarStyle.Literal      => s"=VAL |$value"
            }
      )
      .mkString("\n")

end TestRunnerUtils
