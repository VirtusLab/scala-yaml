package org.virtuslab.yaml.internal.load.reader.token

import org.virtuslab.yaml.Position
import org.virtuslab.yaml.Range
import org.virtuslab.yaml.internal.load.TagHandle
import org.virtuslab.yaml.internal.load.TagPrefix
import org.virtuslab.yaml.internal.load.TagValue
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

final case class Token(kind: TokenKind, range: Range) {
  def start: Position       = range.start
  def end: Option[Position] = range.end
}

sealed abstract class TokenKind
object TokenKind {
  case object StreamStart                                             extends TokenKind
  case object StreamEnd                                               extends TokenKind
  case object DocumentStart                                           extends TokenKind
  case object DocumentEnd                                             extends TokenKind
  case class Anchor(value: String)                                    extends TokenKind
  case class Alias(value: String)                                     extends TokenKind
  case class TagDirective(handle: TagHandle, prefix: TagPrefix)       extends TokenKind
  case class Tag(value: TagValue)                                     extends TokenKind
  case object MappingStart                                            extends TokenKind
  case object SequenceStart                                           extends TokenKind
  case object BlockEnd                                                extends TokenKind
  case object FlowMappingStart                                        extends TokenKind
  case object FlowMappingEnd                                          extends TokenKind
  case object FlowSequenceStart                                       extends TokenKind
  case object FlowSequenceEnd                                         extends TokenKind
  case object SequenceValue                                           extends TokenKind
  case object MappingKey                                              extends TokenKind
  case object MappingValue                                            extends TokenKind
  case object Comma                                                   extends TokenKind
  case class Scalar private (value: String, scalarStyle: ScalarStyle) extends TokenKind

  object Scalar {
    def apply(scalar: String, scalarStyle: ScalarStyle = ScalarStyle.Plain) = {
      val escapedScalar = ScalarStyle.escapeSpecialCharacter(scalar, scalarStyle)
      new Scalar(escapedScalar, scalarStyle)
    }
  }

}
