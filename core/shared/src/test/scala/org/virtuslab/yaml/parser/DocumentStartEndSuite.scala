package org.virtuslab.yaml
package parser

import org.virtuslab.yaml.internal.load.parse.EventKind._
import org.virtuslab.yaml.internal.load.parse.NodeEventMetadata
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class DocumentStartEndSpec extends BaseYamlSuite {

  test("explicit document start") {
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
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("parse empty document") {
    val yaml =
      s"""|""".stripMargin

    val expectedEvents = List(
      StreamStart,
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("explicit document end") {
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
      MappingEnd,
      DocumentEnd(explicit = true),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("implicit document end") {
    val yaml =
      s"""|k1: v1
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("k1"),
      Scalar("v1"),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("document after document end marker") {
    val yaml =
      s"""|---
          |scalar1
          |...
          |key: value
          |
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(explicit = true),
      Scalar("scalar1"),
      DocumentEnd(explicit = true),
      DocumentStart(),
      MappingStart(),
      Scalar("key"),
      Scalar("value"),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("multiple documents with implicit start") {
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
      MappingEnd,
      DocumentEnd(explicit = true),
      DocumentStart(explicit = true),
      MappingStart(),
      Scalar("k2"),
      Scalar("v2"),
      MappingEnd,
      DocumentEnd(explicit = true),
      DocumentStart(explicit = true),
      MappingStart(),
      Scalar("k3"),
      Scalar("v3"),
      MappingEnd,
      DocumentEnd(explicit = true),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("multiple documents with explicit start") {
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
      MappingEnd,
      DocumentEnd(),
      DocumentStart(explicit = true),
      MappingStart(),
      Scalar("k2"),
      Scalar("v2"),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("document start marker at end of input without trailing newline") {
    val yaml = "1\n---"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("1"),
      DocumentEnd(),
      DocumentStart(explicit = true),
      Scalar("", ScalarStyle.Plain, NodeEventMetadata.apply(Tag.nullTag)),
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("document end marker at end of input without trailing newline") {
    val yaml = "1\n..."

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("1"),
      DocumentEnd(explicit = true),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("standalone document start marker without trailing newline") {
    val yaml = "---"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(explicit = true),
      Scalar("", ScalarStyle.Plain, NodeEventMetadata.apply(Tag.nullTag)),
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("standalone document end marker without trailing newline") {
    val yaml = "..."

    val expectedEvents = List(
      StreamStart,
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("scalar followed by document start at end of input is two documents") {
    val yaml = "hello\n---"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("hello"),
      DocumentEnd(),
      DocumentStart(explicit = true),
      Scalar("", ScalarStyle.Plain, NodeEventMetadata.apply(Tag.nullTag)),
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("mapping followed by document start at end of input") {
    val yaml = "k: v\n---"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("k"),
      Scalar("v"),
      MappingEnd,
      DocumentEnd(),
      DocumentStart(explicit = true),
      Scalar("", ScalarStyle.Plain, NodeEventMetadata.apply(Tag.nullTag)),
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }
}
