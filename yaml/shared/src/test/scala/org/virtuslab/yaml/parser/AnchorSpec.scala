package org.virtuslab.yaml
package parser

import org.virtuslab.yaml.internal.load.parse.Anchor
import org.virtuslab.yaml.internal.load.parse.EventKind.*
import org.virtuslab.yaml.internal.load.parse.NodeEventMetadata
import org.virtuslab.yaml.internal.load.reader.Scanner

class AnchorSpec extends BaseYamlSuite:

  test("in mapping") {
    val yaml =
      s"""|First occurrence: &anchor Foo
          |Second occurrence: *anchor
          |Override anchor: &anchor Bar
          |Reuse anchor: *anchor
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("First occurrence"),
      Scalar("Foo", metadata = NodeEventMetadata(Anchor("anchor"))),
      Scalar("Second occurrence"),
      Alias(Anchor("anchor")),
      Scalar("Override anchor"),
      Scalar("Bar", metadata = NodeEventMetadata(Anchor("anchor"))),
      Scalar("Reuse anchor"),
      Alias(Anchor("anchor")),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  // need improvement in tokenizer
  test("in mapping but with keys aliased".ignore) {
    val yaml =
      s"""|&a a: &b b
          |*b : *a
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("a", metadata = NodeEventMetadata(Anchor("a"))),
      Scalar("b", metadata = NodeEventMetadata(Anchor("b"))),
      Alias(Anchor("a")),
      Alias(Anchor("b")),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    yaml.debugTokens
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("in sequence") {
    val yaml =
      s"""|- &a a
          |- &b b
          |- *a
          |- *b
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      Scalar("a", metadata = NodeEventMetadata(Anchor("a"))),
      Scalar("b", metadata = NodeEventMetadata(Anchor("b"))),
      Alias(Anchor("a")),
      Alias(Anchor("b")),
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("as empty values") {
    val yaml = s"""|---
                   |a: &anchor
                   |b: *anchor
                   |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(explicit = true),
      MappingStart(),
      Scalar("a"),
      Scalar("", metadata = NodeEventMetadata(Anchor("anchor"))),
      Scalar("b"),
      Alias(Anchor("anchor")),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("anchor in flow collections".ignore) {
    val yaml =
      s"""|{
          |  a : &b b,
          |  seq: [a, *b]
          |}""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      FlowMappingStart(),
      Scalar("a", metadata = NodeEventMetadata(Anchor("a"))),
      Scalar("b", metadata = NodeEventMetadata(Anchor("b"))),
      Scalar("seq"),
      SequenceStart(),
      Alias(Anchor("a")),
      Alias(Anchor("b")),
      SequenceEnd,
      FlowMappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    yaml.debugTokens
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("anchor & alias".ignore) {
    val yaml =
      s"""|---
          |a: &anchor
          |b: *anchor
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("anchor & alias".ignore) {
    val yaml =
      s"""|---
          |hr:
          |  - Mark McGwire
          |  # Following node labeled SS
          |  - &SS Sammy Sosa
          |rbi:
          |  - *SS # Subsequent occurrence
          |  - Ken Griffey
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }
