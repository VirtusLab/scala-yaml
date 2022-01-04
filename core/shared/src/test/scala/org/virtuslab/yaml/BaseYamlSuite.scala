package org.virtuslab.yaml

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.parse.EventKind
import org.virtuslab.yaml.internal.load.parse.ParserImpl
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.reader.token.TokenKind

trait BaseYamlSuite extends munit.FunSuite {

  def assertTokenEquals(
      obtainedTokens: Either[YamlError, List[TokenKind]],
      expectedTokens: List[TokenKind]
  ) =
    obtainedTokens match {
      case Left(value)   => fail(value.msg)
      case Right(tokens) => assertEquals(tokens, expectedTokens)
    }

  extension (yaml: String)
    def events: Either[YamlError, List[EventKind]] = {
      val reader = Scanner(yaml)
      ParserImpl(reader).getEvents().map(_.map(_.kind))
    }

    def tokens: Either[YamlError, List[TokenKind]] =
      val reader = Scanner(yaml)
      def loop(tokens: List[TokenKind]): Either[YamlError, List[TokenKind]] =
        reader.peekToken().flatMap { t =>
          if t.kind == TokenKind.StreamEnd then Right(tokens.toList)
          else loop(tokens :+ reader.popToken().kind)
        }

      loop(Nil)

    def debugTokens: Unit = pprint.pprintln(tokens)

}
