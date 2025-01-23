package org.virtuslab.yaml

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.parse.EventKind
import org.virtuslab.yaml.internal.load.parse.ParserImpl
import org.virtuslab.yaml.internal.load.reader.Tokenizer
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

  implicit class InternalStringOps(val yaml: String) {
    def events: Either[YamlError, List[EventKind]] = {
      val tokenizer = Tokenizer.make(yaml)
      ParserImpl(tokenizer).getEvents().map(_.map(_.kind))
    }

    def tokens: Either[YamlError, List[TokenKind]] = {
      val tokenizer = Tokenizer.make(yaml)
      def loop(tokens: List[TokenKind]): Either[YamlError, List[TokenKind]] =
        tokenizer.peekToken().flatMap { t =>
          if (t.kind == TokenKind.StreamEnd) Right(tokens.toList)
          else loop(tokens :+ tokenizer.popToken().kind)
        }

      loop(Nil)
    }

    def debugTokens: Unit = pprint.pprintln(tokens)
  }

}
