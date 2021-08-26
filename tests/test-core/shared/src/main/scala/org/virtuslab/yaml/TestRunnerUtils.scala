package org.virtuslab.yaml

import org.virtuslab.yaml.internal.load.parse.Event
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

object TestRunnerUtils {

  def convertEventToYamlTestSuiteFormat(event: List[Event]): String = {
    event
      .map(event =>
        event match {
          case Event.StreamStart                           => "+STR"
          case Event.StreamEnd                             => "-STR"
          case Event.DocumentStart(explicit)               => if (explicit) "+DOC ---" else "+DOC"
          case Event.DocumentEnd(explicit)                 => if (explicit) "-DOC ---" else "-DOC"
          case Event.SequenceStart                         => "+SEQ"
          case Event.SequenceEnd                           => "-SEQ"
          case Event.MappingStart | Event.FlowMappingStart => "+MAP"
          case Event.MappingEnd | Event.FlowMappingEnd     => "-MAP"
          case Event.Scalar(value, style) =>
            style match
              case ScalarStyle.Plain        => s"=VAL :$value"
              case ScalarStyle.DoubleQuoted => s"""=VAL "$value"""
              case ScalarStyle.SingleQuoted => s"=VAL '$value"
              case ScalarStyle.Folded       => s"=VAL >$value"
              case ScalarStyle.Literal      => s"=VAL |$value"
        }
      )
      .mkString("\n")
  }
}
