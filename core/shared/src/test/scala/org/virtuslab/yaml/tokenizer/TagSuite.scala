package org.virtuslab.yaml
package tokenizer

import org.virtuslab.yaml.internal.load.TagHandle
import org.virtuslab.yaml.internal.load.TagPrefix
import org.virtuslab.yaml.internal.load.TagValue
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.TokenKind
import org.virtuslab.yaml.internal.load.reader.token.TokenKind._

class TagSuite extends BaseYamlSuite {

  test("tag directive: primary tag handle") {
    val yaml = """|%TAG ! tag:example.com,2000:app/
                  |---
                  |a
                  |""".stripMargin

    val tokens = List(
      TagDirective(TagHandle.Primary, TagPrefix.Global("tag:example.com,2000:app/")),
      DocumentStart,
      Scalar("a")
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("tag directive: secondary tag handle") {
    val yaml = """|%TAG !! tag:example.com,2000:app/
                  |---
                  |a
                  |""".stripMargin

    val tokens = List(
      TagDirective(TagHandle.Secondary, TagPrefix.Global("tag:example.com,2000:app/")),
      DocumentStart,
      Scalar("a")
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("tag directive: named tag handle & global tag handle") {
    val yaml = """|%TAG !e! tag:example.com,2000:app/
                  |---
                  |a
                  |""".stripMargin

    val tokens = List(
      TagDirective(TagHandle.Named("!e!"), TagPrefix.Global("tag:example.com,2000:app/")),
      DocumentStart,
      Scalar("a")
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("tag directive: local tag") {
    val yaml = """|%TAG !m! !my-
                  |---
                  |a
                  |""".stripMargin

    val tokens = List(
      TagDirective(TagHandle.Named("!m!"), TagPrefix.Local("!my-")),
      DocumentStart,
      Scalar("a")
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("verbatim tag") {
    val yaml = """|---
                  |!<tag:yaml.org,2002:str> foo :
                  |  !<!bar> baz""".stripMargin

    val tokens = List(
      DocumentStart,
      MappingStart,
      MappingKey,
      Tag(TagValue.Verbatim("!<tag:yaml.org,2002:str>")),
      Scalar("foo"),
      MappingValue,
      Tag(TagValue.Verbatim("!<!bar>")),
      Scalar("baz"),
      BlockEnd
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("primary shorthand tag") {
    val yaml = """|!local baz
                  |""".stripMargin

    val tokens = List(
      Tag(TagValue.Shorthand(TagHandle.Primary, "local")),
      Scalar("baz")
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("secondary shorthand tag") {
    val yaml = """|!!str baz
                  |""".stripMargin

    val tokens = List(
      Tag(TagValue.Shorthand(TagHandle.Secondary, "str")),
      Scalar("baz")
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("named shorthand tag, url decoding") {
    val yaml = """|!e!tag%21 baz
                  |""".stripMargin

    val tokens = List(
      Tag(TagValue.Shorthand(TagHandle.Named("!e!"), "tag!")),
      Scalar("baz")
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("non specific tag") {
    val yaml = """|---
                  |! a""".stripMargin

    val tokens = List(
      DocumentStart,
      Tag(TagValue.NonSpecific),
      Scalar("a")
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("plain scalar") {
    val yaml = """|%TAG !! tag:example.com,2000:app/
                  |---
                  |!!int 1 - 3 # Interval, not integer
                  |""".stripMargin

    val tokens = List(
      TagDirective(TagHandle.Secondary, TagPrefix.Global("tag:example.com,2000:app/")),
      DocumentStart,
      Tag(TagValue.Shorthand(TagHandle.Secondary, "int")),
      Scalar("1 - 3")
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("tagged mapping values") {
    val yaml = """|c: !int 42
                  |e: !!str f
                  |g: !e!suffix h
                  |""".stripMargin

    val tokens = List(
      MappingStart,
      MappingKey,
      Scalar("c"),
      MappingValue,
      Tag(TagValue.Shorthand(TagHandle.Primary, "int")),
      Scalar("42"),
      MappingKey,
      Scalar("e"),
      MappingValue,
      Tag(TagValue.Shorthand(TagHandle.Secondary, "str")),
      Scalar("f"),
      MappingKey,
      Scalar("g"),
      MappingValue,
      Tag(TagValue.Shorthand(TagHandle.Named("!e!"), "suffix")),
      Scalar("h"),
      BlockEnd
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("tagged sequence values") {
    val yaml = """|- !str abcd
                  |- !!int 42
                  |- !named!foo bar""".stripMargin

    val tokens = List(
      SequenceStart,
      SequenceValue,
      Tag(TagValue.Shorthand(TagHandle.Primary, "str")),
      Scalar("abcd"),
      SequenceValue,
      Tag(TagValue.Shorthand(TagHandle.Secondary, "int")),
      Scalar("42"),
      SequenceValue,
      Tag(TagValue.Shorthand(TagHandle.Named("!named!"), "foo")),
      Scalar("bar"),
      BlockEnd
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("tagged flow sequence") {
    val yaml = """|!!map {
                  |  k: !!seq
                  |  [ a, b]
                  |}
                  |""".stripMargin

    val tokens = List(
      Tag(TagValue.Shorthand(TagHandle.Secondary, "map")),
      FlowMappingStart,
      MappingKey,
      Scalar("k"),
      MappingValue,
      Tag(TagValue.Shorthand(TagHandle.Secondary, "seq")),
      FlowSequenceStart,
      Scalar("a"),
      Comma,
      Scalar("b"),
      FlowSequenceEnd,
      FlowMappingEnd
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  test("tagged flow sequence") {
    val yaml = """|!!seq [ !!str a, !!int 5]
                  |""".stripMargin

    val tokens = List(
      Tag(TagValue.Shorthand(TagHandle.Secondary, "seq")),
      FlowSequenceStart,
      Tag(TagValue.Shorthand(TagHandle.Secondary, "str")),
      Scalar("a"),
      Comma,
      Tag(TagValue.Shorthand(TagHandle.Secondary, "int")),
      Scalar("5"),
      FlowSequenceEnd
    )

    assertTokenEquals(yaml.tokens, tokens)
  }

  // CUP7.tml
  test("tag with alias") {
    val yaml =
      """|anchored: !local &anchor value
         |!local2 &anchor2 value2 : anchored2
         |""".stripMargin

    val tokens = List(
      MappingStart,
      MappingKey,
      Scalar("anchored"),
      MappingValue,
      Tag(TagValue.Shorthand(TagHandle.Primary, "local")),
      Anchor("anchor"),
      Scalar("value"),
      MappingKey,
      Tag(TagValue.Shorthand(TagHandle.Primary, "local2")),
      Anchor("anchor2"),
      Scalar("value2"),
      MappingValue,
      Scalar("anchored2"),
      BlockEnd
    )
  }

  test("secondary tag with alias") {
    val yaml = """|&anchor !!int 5 : !!str &anchor2 value
                  |""".stripMargin

    val tokens = List(
      MappingStart,
      MappingKey,
      Anchor("anchor"),
      Tag(TagValue.Shorthand(TagHandle.Secondary, "int")),
      Scalar("5"),
      MappingValue,
      Tag(TagValue.Shorthand(TagHandle.Secondary, "str")),
      Anchor("anchor2"),
      Scalar("value"),
      BlockEnd
    )

    assertTokenEquals(yaml.tokens, tokens)
  }
}
