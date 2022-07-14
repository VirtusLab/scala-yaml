package org.virtuslab.yaml
package parser

import org.virtuslab.yaml.internal.load.parse.Anchor
import org.virtuslab.yaml.internal.load.parse.EventKind._
import org.virtuslab.yaml.internal.load.parse.NodeEventMetadata

class AnchorSpec extends BaseYamlSuite {

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

  test("in mapping but with keys aliased") {
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
      Alias(Anchor("b")),
      Alias(Anchor("a")),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
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
      Scalar("", metadata = NodeEventMetadata(Anchor("anchor"), Tag.nullTag)),
      Scalar("b"),
      Alias(Anchor("anchor")),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("anchor in flow collections") {
    val yaml =
      s"""|{
          |  &a a : &b b,
          |  seq: [*a, *b]
          |}""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("a", metadata = NodeEventMetadata(Anchor("a"))),
      Scalar("b", metadata = NodeEventMetadata(Anchor("b"))),
      Scalar("seq"),
      SequenceStart(),
      Alias(Anchor("a")),
      Alias(Anchor("b")),
      SequenceEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("anchor & alias") {
    val yaml =
      s"""|---
          |hr:
          |  - Mark McGwire
          |  # Following node labeled anchor
          |  - &anchor Sammy Sosa
          |rbi:
          |  - *anchor # Subsequent occurrence
          |  - Ken Griffey
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(explicit = true),
      MappingStart(),
      Scalar("hr"),
      SequenceStart(),
      Scalar("Mark McGwire"),
      Scalar("Sammy Sosa", metadata = NodeEventMetadata(Anchor("anchor"))),
      SequenceEnd,
      Scalar("rbi"),
      SequenceStart(),
      Alias(Anchor("anchor")),
      Scalar("Ken Griffey"),
      SequenceEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }
}
