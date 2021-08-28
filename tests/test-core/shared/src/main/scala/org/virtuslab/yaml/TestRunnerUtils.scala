package org.virtuslab.yaml

import org.virtuslab.yaml.internal.load.parse.Event
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

object TestRunnerUtils {

  def convertEventToYamlTestSuiteFormat(event: List[Event]): String = {
    event
      .map(event =>
        event match {
          case Event.StreamStart(_)             => "+STR"
          case Event.StreamEnd(_)               => "-STR"
          case Event.DocumentStart(_, explicit) => if (explicit) "+DOC ---" else "+DOC"
          case Event.DocumentEnd(_, explicit)   => if (explicit) "-DOC ---" else "-DOC"
          case Event.SequenceStart(_)           => "+SEQ"
          case Event.SequenceEnd(_)             => "-SEQ"
          case Event.MappingStart(_) | Event.FlowMappingStart(_) => "+MAP"
          case Event.MappingEnd(_) | Event.FlowMappingEnd(_)     => "-MAP"
          case Event.Scalar(value, style, _) =>
            style match {
              case ScalarStyle.Plain        => s"=VAL :$value"
              case ScalarStyle.DoubleQuoted => s"""=VAL "$value"""
              case ScalarStyle.SingleQuoted => s"=VAL '$value"
              case ScalarStyle.Folded       => s"=VAL >$value"
              case ScalarStyle.Literal      => s"=VAL |$value"
            }
        }
      )
      .mkString("\n")
  }
}
