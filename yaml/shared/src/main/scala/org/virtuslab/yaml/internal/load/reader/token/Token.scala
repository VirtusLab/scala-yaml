package org.virtuslab.yaml.internal.load.reader.token

import org.virtuslab.yaml.Range
import org.virtuslab.yaml.internal.load.TagHandle
import org.virtuslab.yaml.internal.load.TagPrefix
import org.virtuslab.yaml.internal.load.TagValue
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

final case class Token(kind: TokenKind, range: Range)

enum TokenKind:
  case StreamStart
  case StreamEnd
  case DocumentStart
  case DocumentEnd
  case Anchor(value: String)
  case Alias(value: String)
  case TagDirective(handle: TagHandle, prefix: TagPrefix)
  case Tag(value: TagValue)
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
    def apply(scalar: String, scalarStyle: ScalarStyle = ScalarStyle.Plain) =
      val escapedScalar = ScalarStyle.escapeSpecialCharacter(scalar, scalarStyle)
      new Scalar(escapedScalar, scalarStyle)
  end Scalar

end TokenKind
