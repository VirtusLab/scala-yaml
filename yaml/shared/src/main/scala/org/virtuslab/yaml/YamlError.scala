package org.virtuslab.yaml

import org.virtuslab.yaml.internal.load.reader.token.Token

/**
 * An ADT representing a decoding failure.
 */
sealed trait YamlError:
  def msg: String

final case class ParseError(msg: String) extends YamlError
object ParseError:
  def from(expected: String, got: Token): ParseError = ParseError(
    s"Expected $expected but got $got instead of."
  )
  def from(expected: Token, got: Token): ParseError = ParseError.from(expected.toString, got)

final case class ComposerError(msg: String)  extends YamlError
final case class ConstructError(msg: String) extends YamlError
