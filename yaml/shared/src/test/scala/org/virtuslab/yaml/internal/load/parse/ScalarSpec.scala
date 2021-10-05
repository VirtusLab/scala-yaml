package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class ScalarSpec extends BaseParseSuite:

  test("should parse value with special charactar':' as scalar") {
    val yaml =
      s"""|targetPortal: 10.0.2.15:3260:1221:1221
          |iqn: iqn.2001-04.com.example.storage:kube.sys1.xyz
          |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("targetPortal"),
      Scalar("10.0.2.15:3260:1221:1221"),
      Scalar("iqn"),
      Scalar("iqn.2001-04.com.example.storage:kube.sys1.xyz"),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should parse plain scalar value") {
    val yaml =
      s"""| mnt\\#dd
          |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("mnt\\\\#dd", ScalarStyle.Plain),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should parse plain scalar with new lines") {
    val yaml =
      s"""|description: new lines
          |  rest.
          |properties: object
          |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("description", ScalarStyle.Plain),
      Scalar(
        "new lines rest.",
        ScalarStyle.Plain
      ),
      Scalar("properties", ScalarStyle.Plain),
      Scalar("object", ScalarStyle.Plain),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )

    assertEventsEquals(events, expectedEvents)
  }

  test("should parse multiline plain scalar value") {
    val yaml =
      s"""|description: multiline
          |             plain
          |             scalar
          |type: string
          |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("description", ScalarStyle.Plain),
      Scalar("multiline plain scalar", ScalarStyle.Plain),
      Scalar("type", ScalarStyle.Plain),
      Scalar("string", ScalarStyle.Plain),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should parse single quote scalar value with multiline") {
    val yaml =
      s"""description:  'multiline
         |  plain
         |               scalar'
         |type: string
         |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("description", ScalarStyle.Plain),
      Scalar("multiline plain scalar", ScalarStyle.SingleQuoted),
      Scalar("type", ScalarStyle.Plain),
      Scalar("string", ScalarStyle.Plain),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )

    assertEventsEquals(events, expectedEvents)
  }

  test("should parse value with double quote") {
    val yaml =
      s"""| "/mnt/ iscsipd"
          |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("/mnt/ iscsipd", ScalarStyle.DoubleQuoted),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should not espace escape special character in double quote scalar") {
    val yaml = """ "double \n quote" """

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("""double \n quote""", ScalarStyle.DoubleQuoted),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should parse string with single quote") {
    val yaml =
      s"""| '/mnt/ \\iscsipd ''skip'''
          |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("/mnt/ \\\\iscsipd 'skip'", ScalarStyle.SingleQuoted),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should parse single quote with multiline") {
    val yaml =
      s"""|description: 'Quote
          | multiline.'
          |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("description"),
      Scalar("Quote multiline.", ScalarStyle.SingleQuoted),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should parse string as folded scalar") {
    val yaml =
      s"""|command:
          |  - bash
          |  - >-
          |    set -e
          |
          |
          |    test
          |
          |    yaml
          |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("command"),
      SequenceStart(),
      Scalar("bash"),
      Scalar("set -e\\n\\ntest\\nyaml", ScalarStyle.Folded),
      SequenceEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should parse string as literal scalar") {
    val yaml =
      s"""|command:
          |  - bash
          |  - |
          |    # The 
          |    CRARG
          |    # We
          |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("command"),
      SequenceStart(),
      Scalar("bash"),
      Scalar("# The \\nCRARG\\n# We\\n", ScalarStyle.Literal),
      SequenceEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should parse string containing special charactar as scalar with double quote style") {
    val yaml =
      s"""| "{/mnt/ , {}, [] i"
          |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("{/mnt/ , {}, [] i", ScalarStyle.DoubleQuoted),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should parse double quote scalar esceping \" character") {
    val yaml = s""" "{\\" mnt" """

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("{\" mnt", ScalarStyle.DoubleQuoted),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should skip blank lines at the end in folded value") {
    val yaml = s""">
                  | folded
                  | text
                  |
                  |
                  |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("folded text\\n", ScalarStyle.Folded),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should parse new lines for literal style") {

    val yaml = s"""certificate: |-
                  |        -----BEGIN CERTIFICATE-----
                  |        0MTk0MVoXDenkKThvP7IS9q
                  |        +Dzv5hG392KWh5f8xJNs4LbZyl901MeReiLrPH3w=
                  |        -----END CERTIFICATE----
                  |kind: v1
                  |        """.stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("certificate", ScalarStyle.Plain),
      Scalar(
        "-----BEGIN CERTIFICATE-----\\n0MTk0MVoXDenkKThvP7IS9q\\n+Dzv5hG392KWh5f8xJNs4LbZyl901MeReiLrPH3w=\\n-----END CERTIFICATE----",
        ScalarStyle.Literal
      ),
      Scalar("kind", ScalarStyle.Plain),
      Scalar("v1", ScalarStyle.Plain),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should parse new lines for literal style with keep final break") {

    val yaml = s"""certificate: |+
                  |        -----BEGIN CERTIFICATE-----
                  |        0MTk0MVoXDenkKThvP7IS9q
                  |        +Dzv5hG392KWh5f8xJNs4LbZyl901MeReiLrPH3w=
                  |        -----END CERTIFICATE----
                  |
                  |
                  |kind: v1
                  |        """.stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("certificate", ScalarStyle.Plain),
      Scalar(
        "-----BEGIN CERTIFICATE-----\\n0MTk0MVoXDenkKThvP7IS9q\\n+Dzv5hG392KWh5f8xJNs4LbZyl901MeReiLrPH3w=\\n-----END CERTIFICATE----\\n\\n\\n",
        ScalarStyle.Literal
      ),
      Scalar("kind", ScalarStyle.Plain),
      Scalar("v1", ScalarStyle.Plain),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }

  test("should parse literal scalar with multiline") {
    val yaml = s"""key:
                  |  - content: |
                  |     [Unit]
                  |  - content: |
                  |     set -x
                  | 
                  |     """.stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("key"),
      SequenceStart(),
      MappingStart(),
      Scalar("content"),
      Scalar("[Unit]\\n", ScalarStyle.Literal),
      MappingEnd(),
      MappingStart(),
      Scalar("content"),
      Scalar("set -x\\n", ScalarStyle.Literal),
      MappingEnd(),
      SequenceEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(events, expectedEvents)
  }
