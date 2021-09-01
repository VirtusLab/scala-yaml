package org.virtuslab.yaml.internal.load.reader.token

import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.Position

sealed trait Token:
  def pos: Position

case object Token:
  case class StreamStart(pos: Position)       extends Token
  case class StreamEnd(pos: Position)         extends Token
  case class DocumentStart(pos: Position)     extends Token
  case class DocumentEnd(pos: Position)       extends Token
  case class SequenceStart(pos: Position)     extends Token
  case class SequenceEnd(pos: Position)       extends Token
  case class FlowSequenceStart(pos: Position) extends Token
  case class FlowSequenceEnd(pos: Position)   extends Token
  case class MappingStart(pos: Position)      extends Token
  case class FlowMappingStart(pos: Position)  extends Token
  case class MappingEnd(pos: Position)        extends Token
  case class FlowMappingEnd(pos: Position)    extends Token
  case class Key(pos: Position)               extends Token
  case class Value(pos: Position)             extends Token

  final case class Scalar(value: String, scalarStyle: ScalarStyle, pos: Position) extends Token
  case object Scalar:
    def apply(scalar: String, scalarStyle: ScalarStyle, pos: Position): Scalar =
      val escapedScalar = ScalarStyle.escapeSpecialCharacter(scalar, scalarStyle)
      new Scalar(escapedScalar, scalarStyle, pos)
