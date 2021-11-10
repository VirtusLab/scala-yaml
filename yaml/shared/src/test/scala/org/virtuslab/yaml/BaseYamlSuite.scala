package org.virtuslab.yaml

import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.parse.EventKind
import org.virtuslab.yaml.internal.load.parse.ParserImpl
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.reader.token.TokenKind

trait BaseYamlSuite extends munit.FunSuite {

  extension (yaml: String)
    def events: Either[YamlError, List[EventKind]] = {
      val reader = Scanner(yaml)
      ParserImpl(reader).getEvents().map(_.map(_.kind))
    }

    def tokens: List[TokenKind] =
      val reader = Scanner(yaml)
      val tokens = scala.collection.mutable.ArrayDeque[TokenKind]()
      def loop(): List[TokenKind] =
        val t = reader.peekToken().kind
        if t == TokenKind.StreamEnd then tokens.toList
        else
          tokens.append(reader.popToken().kind)
          loop()
      loop()

    def debugTokens: Unit = pprint.pprintln(tokens)

}
