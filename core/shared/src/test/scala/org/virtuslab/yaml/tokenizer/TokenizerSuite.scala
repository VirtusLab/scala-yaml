package org.virtuslab.yaml
package tokenizer

import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.TokenKind
import org.virtuslab.yaml.internal.load.reader.token.TokenKind._

class TokenizerSuite extends BaseYamlSuite {

  test("scalar") {
    val yaml = "aezakmi"

    val tokens = List(
      Scalar("aezakmi", ScalarStyle.Plain)
    )

    assertTokenEquals(yaml.tokens, tokens)
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

    assertTokenEquals(yaml.tokens, tokens)
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

    assertTokenEquals(yaml.tokens, tokens)
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

    assertTokenEquals(yaml.tokens, tokens)
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

    assertTokenEquals(yaml.tokens, tokens)
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

    assertTokenEquals(yaml.tokens, tokens)
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

    assertTokenEquals(yaml.tokens, tokens)
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

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("anchor & alias in mapping") {
    val yaml = """|First occurrence: &anchor Value
                  |Second occurrence: *anchor
                  |Override anchor: &anchor Bar
                  |Reuse anchor: *anchor
                  |""".stripMargin

    val tokens = List(
      MappingStart,
      MappingKey,
      Scalar("First occurrence", ScalarStyle.Plain),
      MappingValue,
      Anchor("anchor"),
      Scalar("Value", ScalarStyle.Plain),
      MappingKey,
      Scalar("Second occurrence", ScalarStyle.Plain),
      MappingValue,
      Alias("anchor"),
      MappingKey,
      Scalar("Override anchor", ScalarStyle.Plain),
      MappingValue,
      Anchor("anchor"),
      Scalar("Bar", ScalarStyle.Plain),
      MappingKey,
      Scalar("Reuse anchor", ScalarStyle.Plain),
      MappingValue,
      Alias("anchor"),
      BlockEnd
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("anchor & alias in sequence") {
    val yaml = """|---
                  |hr:
                  |  - Mark McGwire
                  |  # Following node labeled SS
                  |  - &SS Sammy Sosa
                  |rbi:
                  |  - *SS # Subsequent occurrence
                  |  - Ken Griffey
                  |""".stripMargin

    val tokens = List(
      DocumentStart,
      MappingStart,
      MappingKey,
      Scalar("hr", ScalarStyle.Plain),
      MappingValue,
      SequenceStart,
      SequenceValue,
      Scalar("Mark McGwire", ScalarStyle.Plain),
      SequenceValue,
      Anchor("SS"),
      Scalar("Sammy Sosa", ScalarStyle.Plain),
      BlockEnd,
      MappingKey,
      Scalar("rbi", ScalarStyle.Plain),
      MappingValue,
      SequenceStart,
      SequenceValue,
      Alias("SS"),
      SequenceValue,
      Scalar("Ken Griffey", ScalarStyle.Plain),
      BlockEnd,
      BlockEnd
    )

    assertTokenEquals(yaml.tokens, tokens)
  }
}
