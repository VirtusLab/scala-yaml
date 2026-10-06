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

  test("document markers are not recognized after a mapping value indicator") {
    assertEquals(
      "a: --- x\nb: ... y".events,
      Right(
        List(
          StreamStart,
          DocumentStart(),
          MappingStart(),
          Scalar("a"),
          Scalar("--- x"),
          Scalar("b"),
          Scalar("... y"),
          MappingEnd,
          DocumentEnd(),
          StreamEnd
        )
      )
    )
  }

  test("document markers are not recognized after a sequence entry indicator") {
    assertEquals(
      "- ---\n- ... x".events,
      Right(
        List(
          StreamStart,
          DocumentStart(),
          SequenceStart(),
          Scalar("---"),
          Scalar("... x"),
          SequenceEnd,
          DocumentEnd(),
          StreamEnd
        )
      )
    )
  }

  test("document markers are not recognized inside a plain scalar") {
    assertEquals(
      "foo --- bar ... baz".events,
      Right(
        List(StreamStart, DocumentStart(), Scalar("foo --- bar ... baz"), DocumentEnd(), StreamEnd)
      )
    )
  }

  test("document markers are not recognized right after a document start marker") {
    assertEquals(
      "--- ---\n--- ...".events,
      Right(
        List(
          StreamStart,
          DocumentStart(explicit = true),
          Scalar("---"),
          DocumentEnd(),
          DocumentStart(explicit = true),
          Scalar("..."),
          DocumentEnd(),
          StreamEnd
        )
      )
    )
  }

  test("indented document markers are not recognized in multi-line plain scalars") {
    val yaml =
      s"""|a: b
          |  --- c
          |  ... d
          |e:
          |  ---
          |""".stripMargin

    assertEquals(
      yaml.events,
      Right(
        List(
          StreamStart,
          DocumentStart(),
          MappingStart(),
          Scalar("a"),
          Scalar("b --- c ... d"),
          Scalar("e"),
          Scalar("---"),
          MappingEnd,
          DocumentEnd(),
          StreamEnd
        )
      )
    )
  }

  test("document start marker terminates a not indented literal scalar") {
    val yaml =
      s"""|--- |
          |foo
          |---
          |bar
          |""".stripMargin

    assertEquals(
      yaml.events,
      Right(
        List(
          StreamStart,
          DocumentStart(explicit = true),
          Scalar("foo\n", ScalarStyle.Literal),
          DocumentEnd(),
          DocumentStart(explicit = true),
          Scalar("bar"),
          DocumentEnd(),
          StreamEnd
        )
      )
    )
  }

  test("document end marker terminates a not indented folded scalar") {
    val yaml =
      s"""|--- >
          |foo
          |...
          |""".stripMargin

    assertEquals(
      yaml.events,
      Right(
        List(
          StreamStart,
          DocumentStart(explicit = true),
          Scalar("foo\n", ScalarStyle.Folded),
          DocumentEnd(explicit = true),
          StreamEnd
        )
      )
    )
  }

  test("document start marker right after a block scalar header gives an empty scalar") {
    assertEquals(
      "--- |\n--- >\n...".events,
      Right(
        List(
          StreamStart,
          DocumentStart(explicit = true),
          Scalar("", ScalarStyle.Literal),
          DocumentEnd(),
          DocumentStart(explicit = true),
          Scalar("", ScalarStyle.Folded),
          DocumentEnd(explicit = true),
          StreamEnd
        )
      )
    )
  }

  test("document markers are not allowed inside quoted scalars") {
    assert("'foo\n---\nbar'".events.isLeft)
    assert("'foo\n...\nbar'".events.isLeft)
    assert("\"foo\n---\nbar\"".events.isLeft)
    assert("\"foo\n...\nbar\"".events.isLeft)
    assert("\"foo\\\n...\nbar\"".events.isLeft)
  }

  test("content is not allowed after document end marker") {
    assert("--- a\n... b".events.isLeft)
    assert("... b".events.isLeft)
  }

  test("documents after document end marker without preceding document") {
    val yaml =
      s"""|...\t# comment
          |--- a
          |...
          |---
          |...
          |""".stripMargin

    assertEquals(
      yaml.events,
      Right(
        List(
          StreamStart,
          DocumentStart(explicit = true),
          Scalar("a"),
          DocumentEnd(explicit = true),
          DocumentStart(explicit = true),
          Scalar("", ScalarStyle.Plain, NodeEventMetadata.apply(Tag.nullTag)),
          DocumentEnd(explicit = true),
          StreamEnd
        )
      )
    )
  }

  test("block mapping is not allowed on the document start line") {
    assert("--- a: b".events.isLeft)
    assert("--- &anchor a: b".events.isLeft)
    assert("--- key1: value1\n    key2: value2".events.isLeft)
  }
}
