package org.virtuslab.internal.load.reader.token

import org.virtuslab.internal.load.reader.token.ScalarStyle

sealed trait Token

case object Token:

  case object StreamStart   extends Token
  case object StreamEnd     extends Token
  case object DocumentStart extends Token
  case object DocumentEnd   extends Token
  case object SequenceStart extends Token
  case object SequenceEnd   extends Token
  case object MappingStart  extends Token
  case object MappingEnd    extends Token
  case object Empty         extends Token

  case class Scalar(value: String, scalarStyle: ScalarStyle) extends Token
  case object Scalar:
    def from(value: String): Scalar = value match
      case s""""$v"""" => Scalar(v, ScalarStyle.DoubleQuoted)
      case s"'$v'"     => Scalar(v, ScalarStyle.SingleQuoted)
      case v           => Scalar(v, ScalarStyle.Plain)
