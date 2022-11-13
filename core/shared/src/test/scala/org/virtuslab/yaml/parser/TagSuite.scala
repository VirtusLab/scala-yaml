package org.virtuslab.yaml
package parser

import org.virtuslab.yaml.internal.load.parse.Anchor
import org.virtuslab.yaml.internal.load.parse.EventKind
import org.virtuslab.yaml.internal.load.parse.EventKind._
import org.virtuslab.yaml.internal.load.parse.NodeEventMetadata
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class TagSuite extends BaseYamlSuite {
  // from test 6WLZ
  test("two documents and global tag") {
    val yaml = """|!foo "bar"
                  |...
                  |# Global
                  |%TAG ! tag:example.com,2000:app/
                  |---
                  |!foo "bar"
                  |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("bar", ScalarStyle.DoubleQuoted, NodeEventMetadata(CustomTag("!foo"))),
      DocumentEnd(explicit = true),
      DocumentStart(explicit = true),
      Scalar(
        "bar",
        ScalarStyle.DoubleQuoted,
        NodeEventMetadata(CustomTag("tag:example.com,2000:app/foo"))
      ),
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  // from test 9HCY
  test("scalar before directive") {
    val yaml = """|!foo "bar"
                  |%TAG ! tag:example.com,2000:app/
                  |---
                  |!foo "bar"
                  |""".stripMargin
    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("bar", ScalarStyle.DoubleQuoted, NodeEventMetadata(CustomTag("!foo"))),
      DocumentEnd(),
      DocumentStart(explicit = true),
      Scalar(
        "bar",
        ScalarStyle.DoubleQuoted,
        NodeEventMetadata(CustomTag("tag:example.com,2000:app/foo"))
      ),
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  // from test 6CK3
  test("primary, secondary and named tag handle") {
    val yaml = """|%TAG !e! tag:example.com,2000:app/
                  |---
                  |- !local foo
                  |- !!str bar
                  |- !e!tag%21 baz
                  |""".stripMargin
    val expectedEvents = List(
      StreamStart,
      DocumentStart(explicit = true),
      SequenceStart(),
      Scalar("foo", metadata = NodeEventMetadata(CustomTag("!local"))),
      Scalar("bar", metadata = NodeEventMetadata(CoreSchemaTag("tag:yaml.org,2002:str"))),
      Scalar("baz", metadata = NodeEventMetadata(CustomTag("tag:example.com,2000:app/tag!"))),
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  // from test 8MK2
  test("plain scalar with non specific tag") {
    val yaml = """|! a
                  |""".stripMargin
    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("a"),
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  // from test J7PZ
  test("ordered mapping tag") {
    val yaml = """|--- !!omap
                  |- Mark McGwire: 65
                  |- Sammy Sosa: 63
                  |- Ken Griffy: 58
                  |""".stripMargin
    val expectedEvents = List(
      StreamStart,
      DocumentStart(explicit = true),
      SequenceStart(NodeEventMetadata(CustomTag("tag:yaml.org,2002:omap"))),
      MappingStart(),
      Scalar("Mark McGwire"),
      Scalar("65"),
      MappingEnd,
      MappingStart(),
      Scalar("Sammy Sosa"),
      Scalar("63"),
      MappingEnd,
      MappingStart(),
      Scalar("Ken Griffy"),
      Scalar("58"),
      MappingEnd,
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  // from test CC74
  test("another named tag") {
    val yaml = """|%TAG !e! tag:example.com,2000:app/
                  |---
                  |- !e!foo "bar"
                  |""".stripMargin
    val expectedEvents = List(
      StreamStart,
      DocumentStart(explicit = true),
      SequenceStart(),
      Scalar(
        "bar",
        ScalarStyle.DoubleQuoted,
        NodeEventMetadata(CustomTag("tag:example.com,2000:app/foo"))
      ),
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  // from test LHL4
  test("invalid tag") {
    val yaml = """|---
                  |!invalid{}tag scalar
                  |""".stripMargin

    assert(yaml.events.isLeft)
  }
}
