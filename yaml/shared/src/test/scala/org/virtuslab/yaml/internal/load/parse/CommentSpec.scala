package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.reader.Scanner

class CommentSpec extends BaseParseSuite:

  test("ignore-comment") {
    val yaml =
      s"""|#Comment.
          |
          |apiVersion: apps/v1  app # comment
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("apiVersion"),
      Scalar("apps/v1  app"),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("empty-document") {
    val yaml =
      s"""|#Comment.""".stripMargin

    val expectedEvents = List(
        StreamStart,
        DocumentStart(),
        Scalar(""),
        DocumentEnd(),
        StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("comments-in-mapping") {
    val yaml =
      s"""|spec:
          |  # comment or delete
          |  type: NodePort
          |  # if your cluster supports it, uncomment the following to automatically create
          |  # an external load-balanced IP for the frontend service.
          |  # type: LoadBalancer
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("spec"),
      MappingStart(),
      Scalar("type"),
      Scalar("NodePort"),
      MappingEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }
