package org.virtuslab.yaml

import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.{ParseError, YamlError}

sealed trait YamlError:
  def msg: String

case class ParseError(msg: String) extends YamlError
object ParseError:
  def from(expected: String, got: Token): ParseError = ParseError(
    s"Expected $expected but got $got instead of."
  )
  def from(expected: Token, got: Token): ParseError = ParseError.from(expected.toString, got)

case class ComposerError(msg: String)  extends YamlError
case class ConstructError(msg: String) extends YamlError
