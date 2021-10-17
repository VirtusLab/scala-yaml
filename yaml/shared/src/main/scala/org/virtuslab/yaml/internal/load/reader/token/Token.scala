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
  case Comma
  case Scalar private (value: String, scalarStyle: ScalarStyle)

object TokenKind:
  object Scalar:
    def apply(scalar: String, scalarStyle: ScalarStyle) =
      val escapedScalar = ScalarStyle.escapeSpecialCharacter(scalar, scalarStyle)
      new Scalar(escapedScalar, scalarStyle)
  end Scalar

end TokenKind
