package org.virtuslab.internal.load.parse

import org.virtuslab.internal.load.parse.Event._
import org.virtuslab.internal.load.reader.YamlReader

class CommentSpec extends munit.FunSuite:

  test("should parse key value with ignoring comments") {
    val yaml =
      s"""|#Comment.
          |
          |apiVersion: apps/v1  app # comment
          |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("apiVersion"),
        Scalar("apps/v1  app"),
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse empty document".ignore) {
    val yaml =
      s"""|#Comment.
          |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse mapping with comments") {
    val yaml =
      s"""|spec:
          |  # comment or delete
          |  type: NodePort
          |  # if your cluster supports it, uncomment the following to automatically create
          |  # an external load-balanced IP for the frontend service.
          |  # type: LoadBalancer
          |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("spec"),
        MappingStart,
        Scalar("type"),
        Scalar("NodePort"),
        MappingEnd,
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }
