package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class MappingSpec extends BaseParseSuite:

  test("should parse empty mapping") {
    val yaml = "emptyDir: {}"

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
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("should parse mapping with double quote key") {
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

  test("should parse nested empty mapping") {
    val yaml = "emptyDir: {{{}}}"

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
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("should parse mapping with empty value") {
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

  test("should parse mapping with empty value and comnent") {
    val yaml = s"""key:
                  |
                  |# Commnet.
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

  test("should parse maping of mappings with {...}") {
    val yaml = "hostPath: {key: value, path: /dev/log}"

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
    assertEventsEquals(yaml.events, expectedEvents)
  }
  test("should parse maping key value with } brackets") {
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
