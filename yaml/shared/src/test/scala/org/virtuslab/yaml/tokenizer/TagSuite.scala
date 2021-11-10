package org.virtuslab.yaml
package tokenizer

import org.virtuslab.yaml.*
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.TokenKind
import org.virtuslab.yaml.internal.load.reader.token.TokenKind.*
import org.virtuslab.yaml.internal.load.TagHandle
import org.virtuslab.yaml.internal.load.TagValue
import org.virtuslab.yaml.internal.load.TagPrefix

class TagSuite extends BaseYamlSuite:

  test("tag directive: primary tag handle".only) {
    val yaml = """|%TAG ! tag:example.com,2000:app/
                  |---
                  |a
                  |""".stripMargin

    val tokens = List(
      TagDirective(TagHandle.Primary, TagPrefix.Global("tag:example.com,2000:app/")),
      DocumentStart,
      Scalar("a")
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("tag directive: secondary tag handle".only) {
    val yaml = """|%TAG !! tag:example.com,2000:app/
                  |---
                  |a
                  |""".stripMargin

    val tokens = List(
      TagDirective(TagHandle.Secondary, TagPrefix.Global("tag:example.com,2000:app/")),
      DocumentStart,
      Scalar("a")
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("tag directive: named tag handle & global tag handle".only) {
    val yaml = """|%TAG !e! tag:example.com,2000:app/
                  |---
                  |a
                  |""".stripMargin

    val tokens = List(
      TagDirective(TagHandle.Named("!e!"), TagPrefix.Global("tag:example.com,2000:app/")),
      DocumentStart,
      Scalar("a")
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("tag directive: local tag".only) {
    val yaml = """|%TAG !m! !my-
                  |---
                  |a
                  |""".stripMargin

    val tokens = List(
      TagDirective(TagHandle.Named("!m!"), TagPrefix.Local("!my-")),
      DocumentStart,
      Scalar("a")
    )

    assertEquals(yaml.tokens, tokens)
  }

  // todo Tag token should be placed after MappingKey
  test("verbatim tag".only) {
    val yaml = """|---
                  |!<tag:yaml.org,2002:str> foo :
                  |  !<!bar> baz""".stripMargin

    val tokens = List(
      DocumentStart,
      /*fix this*/ Tag(TagValue.Verbatim("!<tag:yaml.org,2002:str>")),
      MappingStart,
      MappingKey,
      //should be Tag(TagValue.Verbatim("!<tag:yaml.org,2002:str>")),
      Scalar("foo"),
      MappingValue,
      /*fix this*/ BlockEnd,
      Tag(TagValue.Verbatim("!<!bar>")),
      Scalar("baz")
      //should be BlockEnd,
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("primary shorthand tag".only) {
    val yaml = """|!local baz
                  |""".stripMargin

    val tokens = List(
      Tag(TagValue.Shorthand(TagHandle.Primary, "local")),
      Scalar("baz"),
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("secondary shorthand tag".only) {
    val yaml = """|!!str baz
                  |""".stripMargin

    val tokens = List(
      Tag(TagValue.Shorthand(TagHandle.Secondary, "str")),
      Scalar("baz"),
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("named shorthand tag".only) {
    val yaml = """|!e!tag%21 baz
                  |""".stripMargin

    val tokens = List(
      Tag(TagValue.Shorthand(TagHandle.Named("!e!"), "tag%21")),
      Scalar("baz"),
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("non specific tag".only) {
    val yaml = """|---
                  |! a""".stripMargin

    val tokens = List(
      DocumentStart,
      Tag(TagValue.NonSpecific),
      Scalar("a")
    )

    assertEquals(yaml.tokens, tokens)
  }

  test("plain scalar".only) {
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

    assertEquals(yaml.tokens, tokens)
  }

// test("tag in flow sequence nested in mapping".ignore) {
//   val yaml = """|!!map {
//                     |  k: !!seq
//                     |  [ a, !!str b]
//                     |}
//                     |""".stripMargin

//   val tokens = List(
//     Tag("!!map"),
//     FlowMappingStart,
//     MappingKey,
//     Scalar("k"),
//     MappingValue,
//     Tag("!!seq"),
//     FlowSequenceStart,
//     Scalar("a"),
//     Comma,
//     Tag("!!str"),
//     Scalar("b"),
//     FlowSequenceEnd,
//     FlowMappingEnd
//   )

//   assertEquals(yaml.tokens, tokens)
// }

// test("flow sequence mapping nested".ignore.ignore) {
//   val yaml = """|- !!str
//                     |-
//                     |  !!null : a
//                     |  b: !!str
//                     |- !!str : !!null
//                     |""".stripMargin

//   val tokens = List(
//   )

//   assertEquals(yaml.tokens, tokens)
// }

// test("tag in mapping".ignore.ignore) {
//   val yaml = """|!<tag:yaml.org,2002:str> foo :
//                     |  !<!bar> baz
//                     |""".stripMargin

//   val tokens = List(
//     Tag("!<tag:yaml.org,2002:str>"),
//     MappingStart,
//     MappingKey,
//     Scalar("foo"),
//     MappingValue,
//     Tag("!<!bar>"),
//     Scalar("baz"),
//     BlockEnd
//   )

//   assertEquals(yaml.tokens, tokens)
// }

// test("flow sequence mapping nested".ignore.ignore) {
//   val yaml = """|!!str a: b
//                     |c: !!int 42
//                     |e: !!str f
//                     |g: h
//                     |!!str 23: !!bool false
//                     |""".stripMargin

//   val tokens = List(
//   )

//   assertEquals(yaml.tokens, tokens)
// }

// test("flow sequence mapping nested".ignore.ignore) {
//   val yaml = """|anchored: !local &anchor value
//                     |alias: *anchor
//                     |""".stripMargin

//   val tokens = List(
//   )

//   assertEquals(yaml.tokens, tokens)
// }

// test("flow sequence mapping nested".ignore.ignore) {
//   val yaml = """|%TAG !e! tag:example.com,2000:app/
//                     |---
//                     |!e!foo "bar"""".stripMargin

//   val tokens = List(
//   )

//   assertEquals(yaml.tokens, tokens)
// }
