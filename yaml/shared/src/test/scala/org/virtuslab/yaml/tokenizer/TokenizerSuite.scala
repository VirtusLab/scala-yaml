package org.virtuslab.yaml.tokenizer

import org.virtuslab.yaml.BaseYamlSuite
import org.virtuslab.yaml.*
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.TokenKind
import org.virtuslab.yaml.internal.load.reader.token.TokenKind.*

class TokenizerSuite extends BaseYamlSuite:

  test("scalar") {
    val yaml = "aezakmi"

    val tokens = List(
      Scalar("aezakmi", ScalarStyle.Plain)
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("block mapping") {
    val yaml = "k : v"

    val tokens = List(
      MappingStart,
      MappingKey,
      Scalar("k", ScalarStyle.Plain),
      MappingValue,
      Scalar("v", ScalarStyle.Plain),
      BlockEnd
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("block-sequence") {
    val yaml = """|- v1
                  |- v2
                  |""".stripMargin

    val tokens = List(
      SequenceStart,
      SequenceValue,
      Scalar("v1", ScalarStyle.Plain),
      SequenceValue,
      Scalar("v2", ScalarStyle.Plain),
      BlockEnd
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("flow mapping") {
    val yaml = """|{ v1 : v2, }
                  |""".stripMargin

    val tokens = List(
      FlowMappingStart,
      MappingKey,
      Scalar("v1", ScalarStyle.Plain),
      MappingValue,
      Scalar("v2", ScalarStyle.Plain),
      Comma,
      FlowMappingEnd
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("flow mapping implicit values") {
    val yaml = """|{ 
                  |k1,
                  |k2 ,
                  |k3: ,
                  |k4:,
                  |
                  |}
                  |""".stripMargin

    val tokens = List(
      FlowMappingStart,
      Scalar("k1", ScalarStyle.Plain),
      Comma,
      Scalar("k2", ScalarStyle.Plain),
      Comma,
      MappingKey,
      Scalar("k3", ScalarStyle.Plain),
      MappingValue,
      Comma,
      MappingKey,
      Scalar("k4", ScalarStyle.Plain),
      MappingValue,
      Comma,
      FlowMappingEnd
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("flow mapping implicit key") {
    val yaml = """|{ 
                  |: v1 
                  |}
                  |""".stripMargin

    val tokens = List(
      FlowMappingStart,
      MappingKey,
      Scalar("", ScalarStyle.Plain),
      MappingValue,
      Scalar("v1", ScalarStyle.Plain),
      FlowMappingEnd
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("flow sequence") {
    val yaml = """|[ v1, v2, ]
                  |""".stripMargin

    val tokens = List(
      FlowSequenceStart,
      Scalar("v1", ScalarStyle.Plain),
      Comma,
      Scalar("v2", ScalarStyle.Plain),
      Comma,
      FlowSequenceEnd
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("flow sequence mapping") {
    val yaml = """|[ k1: v2, ]
                  |""".stripMargin

    val tokens = List(
      FlowSequenceStart,
      MappingKey,
      Scalar("k1", ScalarStyle.Plain),
      MappingValue,
      Scalar("v2", ScalarStyle.Plain),
      Comma,
      FlowSequenceEnd
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("flow sequence mapping") {
    val yaml = s"""|[
                   |[ nested ],
                   |single: pair
                   |]
                   |""".stripMargin

    val tokens = List(
      FlowSequenceStart,
      FlowSequenceStart,
      Scalar("nested", ScalarStyle.Plain),
      FlowSequenceEnd,
      Comma,
      MappingKey,
      Scalar("single", ScalarStyle.Plain),
      MappingValue,
      Scalar("pair", ScalarStyle.Plain),
      FlowSequenceEnd
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("flow sequence mapping nested") {
    val yaml = """|[ k1: k2: plain
                  |    text ]
                  |""".stripMargin

    val tokens = List(
      FlowSequenceStart,
      MappingKey,
      Scalar("k1", ScalarStyle.Plain),
      MappingValue,
      MappingKey,
      Scalar("k2", ScalarStyle.Plain),
      MappingValue,
      Scalar("plain text", ScalarStyle.Plain),
      FlowSequenceEnd
    )

    assertEquals(yaml.tokens, tokens)
  }
