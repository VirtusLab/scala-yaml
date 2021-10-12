package org.virtuslab.yaml.tokenizer

import org.virtuslab.yaml.*
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.reader.token.TokenKind
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.parse.Event.MappingStart

extension (yaml: String)
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

  def debugTokens: Unit =
    println(yaml.tokens)

class TokenizerSuite extends munit.FunSuite:

  test("scalar") {
    val yaml = "aezakmi"

    val tokens = List(
      TokenKind.Scalar("aezakmi", ScalarStyle.Plain)
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("block mapping") {
    val yaml = "k : v"

    val tokens = List(
      TokenKind.MappingStart,
      TokenKind.MappingKey,
      TokenKind.Scalar("k", ScalarStyle.Plain),
      TokenKind.MappingValue,
      TokenKind.Scalar("v", ScalarStyle.Plain),
      TokenKind.BlockEnd
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("block-sequence") {
    val yaml = """|- v1
                  |- v2
                  |""".stripMargin

    val tokens = List(
      TokenKind.SequenceStart,
      TokenKind.SequenceValue,
      TokenKind.Scalar("v1", ScalarStyle.Plain),
      TokenKind.SequenceValue,
      TokenKind.Scalar("v2", ScalarStyle.Plain),
      TokenKind.BlockEnd
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("flow mapping") {
    val yaml = """|{ v1 : v2, }
                  |""".stripMargin

    val tokens = List(
      TokenKind.FlowMappingStart,
      TokenKind.MappingKey,
      TokenKind.Scalar("v1", ScalarStyle.Plain),
      TokenKind.MappingValue,
      TokenKind.Scalar("v2", ScalarStyle.Plain),
      TokenKind.FlowMappingEnd
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("flow sequence") {
    val yaml = """|[ v1, v2, ]
                  |""".stripMargin

    val tokens = List(
      TokenKind.FlowSequenceStart,
      TokenKind.Scalar("v1", ScalarStyle.Plain),
      TokenKind.Scalar("v2", ScalarStyle.Plain),
      TokenKind.FlowSequenceEnd
    )

    assertEquals(yaml.tokens, tokens)
  }
