package org.virtuslab.yaml.parser

import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.reader.Scanner

class AnchorSpec extends BaseParseSuite:

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
      Scalar("Foo", anchor = Some("anchor")),
      Scalar("Second occurrence"),
      Alias("anchor"),
      Scalar("Override anchor"),
      Scalar("Bar", anchor = Some("anchor")),
      Scalar("Reuse anchor"),
      Alias("anchor"),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    yaml.debugTokens
    assertEventsEquals(yaml.events, expectedEvents)
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
      Scalar("a", anchor = Some("a")),
      Scalar("b", anchor = Some("b")),
      Alias("a"),
      Alias("b"),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    yaml.debugTokens
    assertEventsEquals(yaml.events, expectedEvents)
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
      Scalar("a", anchor = Some("a")),
      Scalar("b", anchor = Some("b")),
      Alias("a"),
      Alias("b"),
      SequenceEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
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
      Scalar("", anchor = Some("anchor")),
      Scalar("b"),
      Alias("anchor"),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
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
      Scalar("a", anchor = Some("a")),
      Scalar("b", anchor = Some("b")),
      Scalar("seq"),
      SequenceStart(),
      Alias("a"),
      Alias("b"),
      SequenceEnd(),
      FlowMappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    yaml.debugTokens
    assertEventsEquals(yaml.events, expectedEvents)
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
    assertEventsEquals(yaml.events, expectedEvents)
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
    assertEventsEquals(yaml.events, expectedEvents)
  }
