package org.virtuslab.internal.load.parse

import com.eed3si9n.expecty.Expecty.expect
import org.virtuslab.internal.load.parse.Event._
import org.virtuslab.internal.load.reader.YamlReader
import org.virtuslab.internal.load.reader.token.ScalarStyle

class ScalarSpec extends munit.FunSuite {

  test("should parse host:port as scalar") {
    val yaml =
      s"""targetPortal: 10.0.2.15:3260:1221:1221
         |iqn: iqn.2001-04.com.example.storage:kube.sys1.xyz
         |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("targetPortal"),
        Scalar("10.0.2.15:3260:1221:1221"),
        Scalar("iqn"),
        Scalar("iqn.2001-04.com.example.storage:kube.sys1.xyz"),
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

  test("should parse sequence of host:port") {
    val yaml = s"portals: ['10.0.2.16:3260', '10.0.2.17:3260']"

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("portals"),
        SequenceStart,
        Scalar("'10.0.2.16:3260'"),
        Scalar("'10.0.2.17:3260'"),
        SequenceEnd,
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

  test("should parse string with double quote") {
    val yaml =
      s""" "/mnt/ iscsipd"
         |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        Scalar("\"/mnt/ iscsipd\""),
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

  test("should parse string with single quote") {
    val yaml =
      s""" '/mnt/ iscsipd ''skip'''
         |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        Scalar("'/mnt/ iscsipd 'skip''"),
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

  test("should parse string with folded value") {
    val yaml =
      s"""command:
         |  - bash
         |  - >-
         |    set -e
         |    yaml
         |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("command"),
        SequenceStart,
        Scalar("bash"),
        Scalar("set -e yaml", ScalarStyle.Folded),
        SequenceEnd,
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

  test("should parse string with double quote cotains special characters") {
    val yaml =
      s""" "/mnt/ , {}, [] i"
         |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        Scalar("\"/mnt/ , {}, [] i\""),
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

}
