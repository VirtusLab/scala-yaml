package org.virtuslab.internal.load.parse

import com.eed3si9n.expecty.Expecty.expect
import org.virtuslab.internal.load.parse.Event._
import org.virtuslab.internal.load.reader.YamlReader

class MappingSpec extends munit.FunSuite {

  test("should parse empty mapping") {
    val yaml   = "emptyDir: {}"
    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("emptyDir"),
        FlowMappingStart,
        FlowMappingEnd,
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

  test("should parse nested empty mapping") {
    val yaml   = "emptyDir: {{{}}}"
    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("emptyDir"),
        FlowMappingStart,
        FlowMappingStart,
        FlowMappingStart,
        FlowMappingEnd,
        Scalar(""),
        FlowMappingEnd,
        Scalar(""),
        FlowMappingEnd,
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

  test("should parse mapping with empty value") {
    val yaml =
      s"""key: 
         |key2: value
         |""".stripMargin
    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("key"),
        Scalar(""),
        Scalar("key2"),
        Scalar("value"),
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

  test("should parse mapping with empty value and comnent") {
    val yaml = s"""key:
                  |
                  |# Commnet.
                  |period: 10""".stripMargin
    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("key"),
        Scalar(""),
        Scalar("period"),
        Scalar("10"),
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

  test("should parse yaml with template value") {

    val yaml   = "replicas: {{replicas}}"
    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("replicas"),
        FlowMappingStart,
        FlowMappingStart,
        Scalar("replicas"),
        Scalar(""),
        FlowMappingEnd,
        Scalar(""),
        FlowMappingEnd,
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

  test("should parse maping of mappings with {...}") {
    val yaml   = "hostPath: {path: /dev/log}"
    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("hostPath"),
        FlowMappingStart,
        Scalar("path"),
        Scalar("/dev/log"),
        FlowMappingEnd,
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }
  test("should parse maping key value with } brackets") {
    val yaml   = "name: etcd-{{cell}}"
    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("name"),
        Scalar("etcd-{{cell}}"),
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }
}
