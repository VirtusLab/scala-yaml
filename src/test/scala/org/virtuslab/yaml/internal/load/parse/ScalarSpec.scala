package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.reader.YamlReader
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class ScalarSpec extends munit.FunSuite:

  test("should parse value with special charactar':' as scalar") {
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
    assertEquals(events, expectedEvents)
  }

  test("should parse plain scalar value") {
    val yaml =
      s""" mnt\\#dd
         |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        Scalar("mnt\\\\#dd", ScalarStyle.Plain),
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse value with double quote") {
    val yaml =
      s""" "/mnt/ iscsipd"
         |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        Scalar("/mnt/ iscsipd", ScalarStyle.DoubleQuoted),
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse string with single quote") {
    val yaml =
      s""" '/mnt/ \\iscsipd ''skip'''
         |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        Scalar("/mnt/ \\\\iscsipd 'skip'", ScalarStyle.SingleQuoted),
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse single quote with multiline") {
    val yaml =
      s"""description: 'Quote 
         | multiline.'
         |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        MappingStart,
        Scalar("description"),
        Scalar("Quote multiline.", ScalarStyle.SingleQuoted),
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse string as folded scalar") {
    val yaml =
      s"""command:
         |  - bash
         |  - >-
         |    set -e
         |
         |
         |    test
         |
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
        Scalar("set -e\\n\\ntest\\nyaml", ScalarStyle.Folded),
        SequenceEnd,
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse string as literal scalar") {
    val yaml =
      s"""command:
         |  - bash
         |  - |
         |    # The 
         |    CRARG
         |    # We
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
        Scalar("# The \\nCRARG\\n# We\\n", ScalarStyle.Literal),
        SequenceEnd,
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse string containing special charactar as scalar with double quote style") {
    val yaml =
      s""" "{/mnt/ , {}, [] i"
         |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        Scalar("{/mnt/ , {}, [] i", ScalarStyle.DoubleQuoted),
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse double quote scalar esceping \" character") {
    val yaml = s""" "{\\" mnt" """

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        Scalar("{\" mnt", ScalarStyle.DoubleQuoted),
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }

  test("should parse double quote scalar esceping \" character2") {
    val yaml = s""" "{\\" mnt" """

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        Scalar("{\" mnt", ScalarStyle.DoubleQuoted),
        DocumentEnd(),
        StreamEnd
      )
    )
    assertEquals(events, expectedEvents)
  }
