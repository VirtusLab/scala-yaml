package org.virtuslab.yaml
package parser

import org.virtuslab.yaml.internal.load.parse.EventKind._
import org.virtuslab.yaml.internal.load.parse.NodeEventMetadata
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class MappingSuite extends BaseYamlSuite {

  test("basic mapping") {
    val yaml =
      s"""hr:  65
         |avg: 0.278
         |rbi: 147""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("hr"),
      Scalar("65"),
      Scalar("avg"),
      Scalar("0.278"),
      Scalar("rbi"),
      Scalar("147"),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("nested mapping") {
    val yaml =
      s"""|key1:
          |  nestedKey1: value1
          |key2:
          |  nestedKey2: value2
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("key1"),
      MappingStart(),
      Scalar("nestedKey1"),
      Scalar("value1"),
      MappingEnd,
      Scalar("key2"),
      MappingStart(),
      Scalar("nestedKey2"),
      Scalar("value2"),
      MappingEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("mapping of sequence") {
    val yaml =
      s"""|command:
          |    - /bin/sh
          |    - -c
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("command"),
      SequenceStart(),
      Scalar("/bin/sh"),
      Scalar("-c"),
      SequenceEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("mappings of sequence") {
    val yaml = s"""american:
                  |  - Boston Red Sox
                  |  - Detroit Tigers
                  |  - New York Yankees
                  |national:
                  |  - New York Mets
                  |  - Chicago Cubs
                  |  - Atlanta Braves
                  |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("american"),
      SequenceStart(),
      Scalar("Boston Red Sox"),
      Scalar("Detroit Tigers"),
      Scalar("New York Yankees"),
      SequenceEnd,
      Scalar("national"),
      SequenceStart(),
      Scalar("New York Mets"),
      Scalar("Chicago Cubs"),
      Scalar("Atlanta Braves"),
      SequenceEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("mapping quoted key") {
    val yaml =
      s"""|data:
          |  "19": xw==
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("data"),
      MappingStart(),
      Scalar("19", ScalarStyle.DoubleQuoted),
      Scalar("xw=="),
      MappingEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("mapping single quoted key") {
    val yaml =
      s"""|'key': value
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("key", style = ScalarStyle.SingleQuoted),
      Scalar("value"),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("mapping empty value") {
    val yaml =
      s"""key: 
          |key2: value
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("key"),
      Scalar("", metadata = NodeEventMetadata(Tag.nullTag)),
      Scalar("key2"),
      Scalar("value"),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("mapping empty value and comment") {
    val yaml = s"""key:
                  |
                  |# Comment.
                  |period: 10""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("key"),
      Scalar("", metadata = NodeEventMetadata(Tag.nullTag)),
      Scalar("period"),
      Scalar("10"),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("flow mapping") {
    val yaml = s"""doubles: { double1: 1.0, double2: 2.0 }""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("doubles"),
      MappingStart(),
      Scalar("double1"),
      Scalar("1.0"),
      Scalar("double2"),
      Scalar("2.0"),
      MappingEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("mapping with braces in value") {
    val yaml = "name: etcd-{{cell}}"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("name"),
      Scalar("etcd-{{cell}}"),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("template value".ignore) {
    val yaml = "replicas: {{replicas}}"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("replicas"),
      MappingStart(),
      MappingStart(),
      Scalar("replicas"),
      Scalar(""),
      MappingEnd,
      Scalar(""),
      MappingEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("empty flow mapping") {
    val yaml = "{}"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("nested empty flow mapping") {
    val yaml = "{{}}"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      MappingStart(),
      MappingEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("flow mapping with empty flow seq") {
    val yaml = "{[]}"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      SequenceStart(),
      SequenceEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("mapping with scalar as value") {
    val yaml =
      s"""{key: value,}""".stripMargin

    val events = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("key"),
      Scalar("value"),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(events))
  }

  test("mapping with flow mapping as value") {
    val yaml =
      s"""|{ 
          |  {double: 1.0}
          |}""".stripMargin

    val events = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      MappingStart(),
      Scalar("double"),
      Scalar("1.0"),
      MappingEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(events))
  }

  test("flow mapping with flow seq as value") {
    val yaml =
      s"""|{
          |  doubles: [v1, v2, ]
          |}""".stripMargin

    val events = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("doubles"),
      SequenceStart(),
      Scalar("v1"),
      Scalar("v2"),
      SequenceEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(events))
  }

  test("flow mapping with scalar kv pairs") {
    val yaml =
      s"""|{
          |  k1: v1,
          |  k2: v2
          |}""".stripMargin

    val events = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("k1"),
      Scalar("v1"),
      Scalar("k2"),
      Scalar("v2"),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(events))
  }

  test("implicit block key in sequence flow") {
    val yaml =
      s""""implicit block key" : [
           |  "implicit flow key" : value,
           | ]""".stripMargin

    val events = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("implicit block key", style = ScalarStyle.DoubleQuoted),
      SequenceStart(),
      MappingStart(),
      Scalar("implicit flow key", style = ScalarStyle.DoubleQuoted),
      Scalar("value"),
      MappingEnd,
      SequenceEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(events))
  }

  test("mapping scalar with empty lines") {
    val yaml =
      s"""|---
          |plain: a
          | b
          |
          | c
          | 
          | 
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(explicit = true),
      MappingStart(),
      Scalar("plain"),
      Scalar("a b\\nc"),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("double flow mapping") {
    val yaml =
      s"""|---
          |- { "single line", a: b}
          |- { "multi
          |  line", a: b}""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(explicit = true),
      SequenceStart(),
      MappingStart(),
      Scalar("single line", ScalarStyle.DoubleQuoted),
      Scalar("", metadata = NodeEventMetadata(Tag.nullTag)),
      Scalar("a"),
      Scalar("b"),
      MappingEnd,
      MappingStart(),
      Scalar("multi line", ScalarStyle.DoubleQuoted),
      Scalar("", metadata = NodeEventMetadata(Tag.nullTag)),
      Scalar("a"),
      Scalar("b"),
      MappingEnd,
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("skip comment in flom mapping") {
    val yaml =
      s"""|---
          |{ "foo" # comment
          |  :bar }""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(explicit = true),
      MappingStart(),
      Scalar("foo", ScalarStyle.DoubleQuoted),
      Scalar("bar"),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("mapping with folded value") {
    val yaml =
      s"""
         |key: >
         |  value
         |key2: >
         |  value2
         |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("key"),
      Scalar("value\\n", style = ScalarStyle.Folded),
      Scalar("key2"),
      Scalar("value2\\n", style = ScalarStyle.Folded),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("invalid sequence as mapping value") {
    val yaml = """foo: - bar"""

    assert(yaml.events.isLeft)
  }
}
