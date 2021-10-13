package org.virtuslab.yaml.parser

import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class MappingSuite extends BaseParseSuite:

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
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )

    assertEventsEquals(yaml.events, expectedEvents)
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
      MappingEnd(),
      Scalar("key2"),
      MappingStart(),
      Scalar("nestedKey2"),
      Scalar("value2"),
      MappingEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )

    assertEventsEquals(yaml.events, expectedEvents)
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
      SequenceEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
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
      SequenceEnd(),
      Scalar("national"),
      SequenceStart(),
      Scalar("New York Mets"),
      Scalar("Chicago Cubs"),
      Scalar("Atlanta Braves"),
      SequenceEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )

    assertEventsEquals(yaml.events, expectedEvents)
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
      MappingEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
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
      Scalar(""),
      Scalar("key2"),
      Scalar("value"),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
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
      Scalar(""),
      Scalar("period"),
      Scalar("10"),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("mapping with braces in value") {
    val yaml = "name: etcd-{{cell}}"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("name"),
      Scalar("etcd-{{cell}}"),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("template value".ignore) {
    val yaml = "replicas: {{replicas}}"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("replicas"),
      FlowMappingStart(),
      FlowMappingStart(),
      Scalar("replicas"),
      Scalar(""),
      FlowMappingEnd(),
      Scalar(""),
      FlowMappingEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("empty flow mapping") {
    val yaml = "{}"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      FlowMappingStart(),
      FlowMappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("nested empty flow mapping") {
    val yaml = "{{}}"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      FlowMappingStart(),
      FlowMappingStart(),
      FlowMappingEnd(),
      FlowMappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("flow mapping with empty flow seq") {
    val yaml = "{[]}"

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      FlowMappingStart(),
      SequenceStart(),
      SequenceEnd(),
      FlowMappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("mapping with scalar as value") {
    val yaml =
      s"""{key: value,}""".stripMargin

    val events = List(
      StreamStart,
      DocumentStart(),
      FlowMappingStart(),
      Scalar("key"),
      Scalar("value"),
      FlowMappingEnd(),
      DocumentEnd(),
      StreamEnd
    )

    assertEventsEquals(yaml.events, events)
  }

  test("mapping with flow mapping as value") {
    val yaml =
      s"""|{ 
          |  {double: 1.0}
          |}""".stripMargin

    val events = List(
      StreamStart,
      DocumentStart(),
      FlowMappingStart(),
      FlowMappingStart(),
      Scalar("double"),
      Scalar("1.0"),
      FlowMappingEnd(),
      FlowMappingEnd(),
      DocumentEnd(),
      StreamEnd
    )

    assertEventsEquals(yaml.events, events)
  }

  test("flow mapping with flow seq as value") {
    val yaml =
      s"""|{
          |  doubles: [v1, v2, ]
          |}""".stripMargin

    val events = List(
      StreamStart,
      DocumentStart(),
      FlowMappingStart(),
      Scalar("doubles"),
      SequenceStart(),
      Scalar("v1"),
      Scalar("v2"),
      SequenceEnd(),
      FlowMappingEnd(),
      DocumentEnd(),
      StreamEnd
    )

    assertEventsEquals(yaml.events, events)
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
      FlowMappingStart(),
      Scalar("k1"),
      Scalar("v1"),
      Scalar("k2"),
      Scalar("v2"),
      FlowMappingEnd(),
      DocumentEnd(),
      StreamEnd
    )

    assertEventsEquals(yaml.events, events)
  }
