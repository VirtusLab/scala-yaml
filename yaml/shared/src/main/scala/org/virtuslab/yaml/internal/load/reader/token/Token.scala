package org.virtuslab.yaml.internal.load.reader.token

import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.Position

final case class Token(kind: TokenKind, pos: Position)

enum TokenKind:
  case StreamStart
  case StreamEnd
  case DocumentStart
  case DocumentEnd
  case MappingStart
  case SequenceStart
  case BlockEnd
  case FlowMappingStart
  case FlowMappingEnd
  case FlowSequenceStart
  case FlowSequenceEnd
  case SequenceValue
  case MappingKey
  case MappingValue
  case Scalar private (value: String, scalarStyle: ScalarStyle)

  def token(pos: Position): Token = Token(this, pos)

object TokenKind:
  def scalar: Scalar = Scalar("", ScalarStyle.Plain)

  object Scalar:
    def apply(scalar: String, scalarStyle: ScalarStyle) =
      val escapedScalar = ScalarStyle.escapeSpecialCharacter(scalar, scalarStyle)
      new Scalar(escapedScalar, scalarStyle)
  end Scalar

end TokenKind
