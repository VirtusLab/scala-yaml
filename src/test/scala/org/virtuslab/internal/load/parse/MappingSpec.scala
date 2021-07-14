package org.virtuslab.internal.load.parse

import com.eed3si9n.expecty.Expecty.expect
import org.virtuslab.internal.load.parse.Event._
import org.virtuslab.internal.load.reader.YamlReader

class MappingSpec extends munit.FunSuite {

  test("should parse empty mapping".ignore) {
    val yaml   = "emptyDir: {}"
    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("emptyDir"),
        MappingStart,
        MappingEnd,
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

  test("should parse nested empty mapping".ignore) {
    val yaml   = "emptyDir: {{{}}}"
    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("emptyDir"),
        MappingStart,
        MappingStart,
        MappingStart,
        MappingEnd,
        Scalar(""),
        MappingEnd,
        Scalar(""),
        MappingEnd,
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

  test("should parse mapping with empty value".ignore) {
    val yaml   = "key: "
    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("key"),
        Scalar(""),
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

  test("should parse mapping with empty value and comnent".ignore) {
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

  test("should parse yaml with template value".ignore) {
    val yaml   = "replicas: {{replicas}}"
    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("replicas"),
        MappingStart,
        MappingStart,
        Scalar("replicas"),
        Scalar(""),
        MappingEnd,
        Scalar(""),
        MappingEnd,
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
        MappingStart,
        Scalar("path"),
        Scalar("/dev/log"),
        MappingEnd,
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }
  test("should parse maping key value with } brackets".ignore) {
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
