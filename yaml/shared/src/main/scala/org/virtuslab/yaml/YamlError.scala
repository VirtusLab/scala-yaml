package org.virtuslab.yaml

import scala.util.control.NoStackTrace

import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind

/**
 * An ADT representing a decoding failure.
 */
sealed trait YamlError:
  def msg: String

final case class ParseError(msg: String) extends YamlError
object ParseError:
  def from(expected: String, got: Token): ParseError = ParseError(
    s"""|Expected 
        |$expected but instead got ${got.kind}
        |${got.range.errorMsg}""".stripMargin
  )
  def from(expected: TokenKind, got: Token): ParseError = ParseError.from(expected.toString, got)

final case class ComposerError(msg: String)  extends YamlError
final case class ConstructError(msg: String) extends YamlError
final case class ScannerError(msg: String)   extends Throwable with YamlError with NoStackTrace
object ScannerError:
  def from(obtained: String, got: Token): ScannerError = ScannerError(
    s"""|Obtained 
        |$obtained but exptected got ${got.kind}
        |${got.range.errorMsg}""".stripMargin
  )
