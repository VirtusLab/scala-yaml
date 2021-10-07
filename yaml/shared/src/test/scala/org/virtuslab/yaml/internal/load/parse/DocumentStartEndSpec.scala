package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.reader.Scanner

class DocumentStartEndSpec extends BaseParseSuite:

  test("explicit-document-start") {
    val yaml =
      s"""|---
          |k1: v1
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(explicit = true),
      MappingStart(),
      Scalar("k1"),
      Scalar("v1"),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("explicit-document-end") {
    val yaml =
      s"""|k1: v1
          |...
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("k1"),
      Scalar("v1"),
      MappingEnd(),
      DocumentEnd(explicit = true),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("implicit-document-start") {
    val yaml =
      s"""|k1: v1
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("k1"),
      Scalar("v1"),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("multiple-documents-with-implicit-start") {
    val yaml =
      s"""|k1: v1
          |...
          |---
          |k2: v2
          |...
          |---
          |k3: v3
          |...
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("k1"),
      Scalar("v1"),
      MappingEnd(),
      DocumentEnd(explicit = true),
      DocumentStart(explicit = true),
      MappingStart(),
      Scalar("k2"),
      Scalar("v2"),
      MappingEnd(),
      DocumentEnd(explicit = true),
      DocumentStart(explicit = true),
      MappingStart(),
      Scalar("k3"),
      Scalar("v3"),
      MappingEnd(),
      DocumentEnd(explicit = true),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("multiple-documents-with-explicit-start") {
    val yaml =
      s"""|---
          |k1: v1
          |---
          |k2: v2
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(explicit = true),
      MappingStart(),
      Scalar("k1"),
      Scalar("v1"),
      MappingEnd(),
      DocumentEnd(),
      DocumentStart(explicit = true),
      MappingStart(),
      Scalar("k2"),
      Scalar("v2"),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }
