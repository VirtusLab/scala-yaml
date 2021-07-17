package org.virtuslab.internal.load.parse

import org.virtuslab.internal.load.parse.Event._
import org.virtuslab.internal.load.reader.YamlReader

class DocumentStartSpec extends munit.FunSuite:

  test("should parse explicit document start event") {
    val yaml =
      s"""|---
          |k1: v1
          |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(explicit = true),
        MappingStart,
        Scalar("k1"),
        Scalar("v1"),
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse implicit document start event") {
    val yaml =
      s"""|k1: v1
          |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("k1"),
        Scalar("v1"),
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse implicit and multiple explicit document starts") {
    val yaml =
      s"""|k1: v1
          |---
          |k2: v2
          |---
          |k3: v3
          |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(explicit = false),
        MappingStart,
        Scalar("k1"),
        Scalar("v1"),
        MappingEnd,
        DocumentEnd(),
        DocumentStart(explicit = true),
        MappingStart,
        Scalar("k2"),
        Scalar("v2"),
        MappingEnd,
        DocumentEnd(),
        DocumentStart(explicit = true),
        MappingStart,
        Scalar("k3"),
        Scalar("v3"),
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse multiple explicit document's start events") {
    val yaml =
      s"""|---
          |k1: v1
          |---
          |k2: v2
          |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(explicit = true),
        MappingStart,
        Scalar("k1"),
        Scalar("v1"),
        MappingEnd,
        DocumentEnd(),
        DocumentStart(explicit = true),
        MappingStart,
        Scalar("k2"),
        Scalar("v2"),
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }
