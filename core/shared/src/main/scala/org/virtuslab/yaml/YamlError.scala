package org.virtuslab.yaml

import scala.reflect.ClassTag
import scala.util.control.NoStackTrace

import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind
import org.virtuslab.yaml.internal.load.TagValue

/**
 * An ADT representing a decoding failure.
 */
sealed abstract class YamlError(val msg: String) extends Exception(msg)

sealed abstract class ParseError(override val msg: String) extends YamlError(msg)
object ParseError {
  def from(expected: String, got: Token): ParseError    = ExpectedTokenKind(expected, got)
  def from(expected: TokenKind, got: Token): ParseError = ParseError.from(expected.toString, got)

  final case class ExpectedTokenKind(expected: String, got: Token)
      extends ParseError(
        s"""|Expected
            |$expected but instead got ${got.kind}
            |${got.range.errorMsg}""".stripMargin
      )

  final case class NoRegisteredTagDirective(handleKey: String, tokenTag: Token)
      extends ParseError(
        s"There is no registered tag directive for handle $handleKey"
      )
}

final case class ComposerError(override val msg: String) extends YamlError(msg)

final case class ModifyError(override val msg: String) extends YamlError(msg)

final case class ConstructError(
    errorMsg: String,
    node: Option[Node],
    expected: Option[String]
) extends YamlError(node.flatMap(_.pos) match {
      case Some(range) =>
        s"""|$errorMsg
            |at ${range.start.line}:${range.start.column},${expected
             .map(exp => s" expected $exp")
             .getOrElse("")}
            |${range.errorMsg} """.stripMargin
      case None =>
        errorMsg

    })
object ConstructError {
  private def from(
      errorMsg: String,
      node: Option[Node],
      expected: Option[String]
  ): ConstructError = ConstructError(errorMsg, node, expected)
  def from(errorMsg: String, node: Node, expected: String): ConstructError =
    from(errorMsg, Some(node), Some(expected))
  def from(errorMsg: String, expected: String, node: Node): ConstructError =
    from(errorMsg, Some(node), Some(expected))
  def from(errorMsg: String, node: Node): ConstructError = from(errorMsg, Some(node), None)
  def from(errorMsg: String, expected: String): ConstructError =
    from(errorMsg, None, Some(expected))
  def from(errorMsg: String): ConstructError = from(errorMsg, None, None)

  def from(t: Throwable, node: Node, expected: String): ConstructError =
    from(t.getMessage, Some(node), Some(expected))
  def from(t: Throwable, expected: String, node: Node): ConstructError =
    from(t.getMessage, Some(node), Some(expected))
  def from(t: Throwable, node: Node): ConstructError = from(t.getMessage, Some(node), None)
  def from(t: Throwable, expected: String): ConstructError =
    from(t.getMessage, None, Some(expected))
  def from(t: Throwable): ConstructError = from(t.getMessage, None, None)
}

sealed abstract class ScannerError(override val msg: String)
    extends YamlError(msg)
    with NoStackTrace
object ScannerError {
  def from(obtained: String, got: Token): ScannerError = Obtained(obtained, got)

  def from(range: Range, msg: String): ScannerError = AtRange(range, msg)

  case class Obtained(obtained: String, got: Token)
      extends ScannerError(
        s"""|Obtained
            |$obtained but expected got ${got.kind}
            |${got.range.errorMsg}""".stripMargin
      )

  case class AtRange(range: Range, rawMsg: String)
      extends ScannerError(
        s"""|Error at line ${range.start.line}, column ${range.start.column}:
            |${range.errorMsg}
            |$rawMsg
            |""".stripMargin
      )
}
