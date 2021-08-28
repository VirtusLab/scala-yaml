package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class SequenceSpec extends munit.FunSuite:

  test("should parse mapping with sequence value") {
    val yaml =
      s"""command:
         |    - /bin/sh
         |    - -c
         |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart(),
        Scalar("command"),
        SequenceStart(),
        Scalar("/bin/sh"),
        Scalar("-c"),
        SequenceEnd(),
        MappingEnd(),
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse empty flow sequence") {
    val yaml = "seq: []"

    val reader = Scanner(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
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
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse sequence of sequences") {
    val yaml =
      s"""-
         |  - v1
         |  - v2
         |-
         |  - v3
         |  - v4
         |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
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
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse sequence of host:port") {
    val yaml =
      s"""portals: [10.0.2.16:3260, 10.0.2.17:3260]
         |portalsSingleQouta: ['10.0.2.16:3260', '10.0.2.17:3260']
         |portalsDoubleQouta: ["10.0.2.16:3260", "10.0.2.17:3260"]
         |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart(),
        Scalar("portals"),
        SequenceStart(),
        Scalar("10.0.2.16:3260"),
        Scalar("10.0.2.17:3260"),
        SequenceEnd(),
        Scalar("portalsSingleQouta"),
        SequenceStart(),
        Scalar("10.0.2.16:3260", ScalarStyle.SingleQuoted),
        Scalar("10.0.2.17:3260", ScalarStyle.SingleQuoted),
        SequenceEnd(),
        Scalar("portalsDoubleQouta"),
        SequenceStart(),
        Scalar("10.0.2.16:3260", ScalarStyle.DoubleQuoted),
        Scalar("10.0.2.17:3260", ScalarStyle.DoubleQuoted),
        SequenceEnd(),
        MappingEnd(),
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }
