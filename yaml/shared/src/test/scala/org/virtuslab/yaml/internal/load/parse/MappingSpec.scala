package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class MappingSpec extends BaseParseSuite:

  test("basic-mapping") {
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

  test("nested-mapping") {
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

  test("mapping-of-sequence") {
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

  test("mappings-of-sequence") {
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

  test("mapping-quoted-key") {
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

  test("mapping-empty-value") {
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

  test("mapping-empty-value-and-comment") {
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

  test("mapping-with-braces-in-value") {
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

  test("should parse yaml with template value") {
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

  test("should parse empty mapping") {
    val yaml   = "emptyDir: {}"
    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("emptyDir"),
      FlowMappingStart(),
      FlowMappingEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should parse nested empty mapping") {
    val yaml   = "emptyDir: {{{}}}"
    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("emptyDir"),
      FlowMappingStart(),
      FlowMappingStart(),
      FlowMappingStart(),
      FlowMappingEnd(),
      Scalar(""),
      FlowMappingEnd(),
      Scalar(""),
      FlowMappingEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should parse maping of mappings with {...}") {
    val yaml   = "hostPath: {key: value, path: /dev/log}"
    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("hostPath"),
      FlowMappingStart(),
      Scalar("key"),
      Scalar("value"),
      Scalar("path"),
      Scalar("/dev/log"),
      FlowMappingEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }
