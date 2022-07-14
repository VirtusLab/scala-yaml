package org.virtuslab.yaml
package parser

import org.virtuslab.yaml.internal.load.parse.EventKind._
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class SequenceSuite extends BaseYamlSuite {

  test("basic sequence") {
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
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("sequence of sequences") {
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
      SequenceEnd,
      SequenceStart(),
      Scalar("v3"),
      Scalar("v4"),
      SequenceEnd,
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
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
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      MappingStart(),
      Scalar("name"),
      Scalar("Mark McGwire"),
      Scalar("hr"),
      Scalar("65"),
      MappingEnd,
      MappingStart(),
      Scalar("name"),
      Scalar("Sammy Sosa"),
      Scalar("hr"),
      Scalar("63"),
      MappingEnd,
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("indentation sequence") {
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
      SequenceEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("indentation less sequence") {
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
      SequenceEnd,
      Scalar("volumes"),
      SequenceStart(),
      MappingStart(),
      Scalar("name"),
      Scalar("iscsipd3-rw"),
      MappingEnd,
      SequenceEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("empty flow sequence") {
    val yaml = "[]"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("empty nested flow sequence") {
    val yaml = "[[]]"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      SequenceStart(),
      SequenceEnd,
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("empty flow sequence with empty flow mapping") {
    val yaml = "[{}]"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      MappingStart(),
      MappingEnd,
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("sequence of host:port") {
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
      SequenceEnd,
      Scalar("portalsSingleQouta"),
      SequenceStart(),
      Scalar("10.0.2.16:3260", ScalarStyle.SingleQuoted),
      Scalar("10.0.2.17:3260", ScalarStyle.SingleQuoted),
      SequenceEnd,
      Scalar("portalsDoubleQouta"),
      SequenceStart(),
      Scalar("10.0.3.16:3260", ScalarStyle.DoubleQuoted),
      Scalar("10.0.3.17:3260", ScalarStyle.DoubleQuoted),
      SequenceEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("flow sequence with single pair") {
    val yaml = s"""|[
                   |[ nested ],
                   |single: pair
                   |]
                   |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      SequenceStart(),
      Scalar("nested"),
      SequenceEnd,
      MappingStart(),
      Scalar("single"),
      Scalar("pair"),
      MappingEnd,
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("spec flow sequence") {
    val yaml = s"""[
                  |"double
                  | quoted"
                  |]""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      Scalar("double quoted", ScalarStyle.DoubleQuoted),
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("sequence with comma in value") {
    val yaml =
      """
        |- Up, up, and away!
        |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      Scalar("Up, up, and away!"),
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("sequence with double :: in value") {
    val yaml =
      """ - ::value"""

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      Scalar("::value"),
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("flow mapping scalar") {
    val yaml =
      """- { "key"::value } """

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      MappingStart(),
      Scalar("key", style = ScalarStyle.DoubleQuoted),
      Scalar(":value"),
      MappingEnd,
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("colon followed by comma") {
    val yaml =
      """- :,"""

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      Scalar(":,"),
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )
    assertEquals(yaml.events, Right(expectedEvents))
  }
}
