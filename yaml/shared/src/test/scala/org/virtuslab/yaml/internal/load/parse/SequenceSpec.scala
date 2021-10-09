package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class SequenceSpec extends BaseParseSuite:

  test("basic-sequence") {
    val yaml =
      s"""|- v1
          |- v2
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      Scalar("v1"),
      Scalar("v2"),
      SequenceEnd(),
      DocumentEnd(),
      StreamEnd
    )

    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("sequence-of-sequences") {
    val yaml =
      s"""|-
          |  - v1
          |  - v2
          |-
          |  - v3
          |  - v4
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      SequenceStart(),
      Scalar("v1"),
      Scalar("v2"),
      SequenceEnd(),
      SequenceStart(),
      Scalar("v3"),
      Scalar("v4"),
      SequenceEnd(),
      SequenceEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("sequence-of-mappings") {
    val yaml = s"""|-
                   |  name: Mark McGwire
                   |  hr:   65
                   |-
                   |  name: Sammy Sosa
                   |  hr:   63
                   |""".stripMargin

    val expectedEvents = List(
      Event.StreamStart,
      Event.DocumentStart(),
      Event.SequenceStart(),
      Event.MappingStart(),
      Event.Scalar("name"),
      Event.Scalar("Mark McGwire"),
      Event.Scalar("hr"),
      Event.Scalar("65"),
      Event.MappingEnd(),
      Event.MappingStart(),
      Event.Scalar("name"),
      Event.Scalar("Sammy Sosa"),
      Event.Scalar("hr"),
      Event.Scalar("63"),
      Event.MappingEnd(),
      Event.SequenceEnd(),
      Event.DocumentEnd(),
      Event.StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("indentation-sequence") {
    val yaml = s"""|containers:
                   | - name iscsipd1-rw
                   | - name iscsipd2-rw
                   |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("containers"),
      SequenceStart(),
      Scalar("name iscsipd1-rw"),
      Scalar("name iscsipd2-rw"),
      SequenceEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("indentation-less-sequence") {
    val yaml = s"""|containers:
                   |- name iscsipd1-rw
                   |- name iscsipd2-rw
                   |volumes:
                   |- name: iscsipd3-rw
                   |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("containers"),
      SequenceStart(),
      Scalar("name iscsipd1-rw"),
      Scalar("name iscsipd2-rw"),
      SequenceEnd(),
      Scalar("volumes"),
      SequenceStart(),
      MappingStart(),
      Scalar("name"),
      Scalar("iscsipd3-rw"),
      MappingEnd(),
      SequenceEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("empty-flow-sequence") {
    val yaml = "seq: []"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("seq"),
      SequenceStart(),
      SequenceEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("sequence-of-host:port") {
    val yaml =
      s"""|portals: [10.0.1.16:3260, 10.0.1.17:3260]
          |portalsSingleQouta: ['10.0.2.16:3260', '10.0.2.17:3260']
          |portalsDoubleQouta: ["10.0.3.16:3260", "10.0.3.17:3260"]
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("portals"),
      SequenceStart(),
      Scalar("10.0.1.16:3260"),
      Scalar("10.0.1.17:3260"),
      SequenceEnd(),
      Scalar("portalsSingleQouta"),
      SequenceStart(),
      Scalar("10.0.2.16:3260", ScalarStyle.SingleQuoted),
      Scalar("10.0.2.17:3260", ScalarStyle.SingleQuoted),
      SequenceEnd(),
      Scalar("portalsDoubleQouta"),
      SequenceStart(),
      Scalar("10.0.3.16:3260", ScalarStyle.DoubleQuoted),
      Scalar("10.0.3.17:3260", ScalarStyle.DoubleQuoted),
      SequenceEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }
