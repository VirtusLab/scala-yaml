package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class ScalarSpec extends BaseParseSuite:

  test("plain-value") {
    val yaml =
      s"""|mnt\\#dd
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("mnt\\\\#dd", ScalarStyle.Plain),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("single-quote") {
    val yaml =
      s"""| '/mnt/ \\iscsipd ''skip'''
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("/mnt/ \\\\iscsipd 'skip'", ScalarStyle.SingleQuoted),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("double-quote") {
    val yaml =
      s"""|"/mnt/ iscsipd"
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("/mnt/ iscsipd", ScalarStyle.DoubleQuoted),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("unescaped-colon") {
    val yaml =
      s"""|targetPortal: 10.0.2.15:3260:1221:1221
          |iqn: iqn.2001-04.com.example.storage:kube.sys1.xyz
          |""".stripMargin

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
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("with-new-lines") {
    val yaml =
      s"""|description: new lines
          |  rest.
          |properties: object
          |""".stripMargin

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

    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("plain-multiline") {
    val yaml =
      s"""|description: multiline
          |             plain
          |             scalar
          |type: string
          |""".stripMargin

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
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("single-quote-multiline") {
    val yaml =
      s"""description:  'multiline
         |  plain
         |               scalar'
         |type: string
         |""".stripMargin

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

    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("single-quote-multiline-2") {
    val yaml =
      s"""|description: 'Quote
          | multiline.'
          |""".stripMargin

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
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("dont-escape-in-double-quotes") {
    val yaml = """ "double \n quote" """

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("""double \n quote""", ScalarStyle.DoubleQuoted),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("double-quote-special-characters") {
    val yaml =
      s"""| "{/mnt/ , {}, [] i"
          |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("{/mnt/ , {}, [] i", ScalarStyle.DoubleQuoted),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("double-quote-escape-\"character") {
    val yaml = s""" "{\\" mnt" """

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("{\" mnt", ScalarStyle.DoubleQuoted),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("multiline-folded") {
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
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("multiline-folded-skip-lines") {
    val yaml = s""">
                  | folded
                  | text
                  |
                  |
                  |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      Scalar("folded text\\n", ScalarStyle.Folded),
      DocumentEnd(),
      StreamEnd
    )
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("multiline-literal-1") {
    val yaml =
      s"""|command:
          |  - bash
          |  - |
          |    # The 
          |    CRARG
          |    # We
          |""".stripMargin

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
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("multiline-literal-2") {

    val yaml = s"""certificate: |-
                  |        -----BEGIN CERTIFICATE-----
                  |        0MTk0MVoXDenkKThvP7IS9q
                  |        +Dzv5hG392KWh5f8xJNs4LbZyl901MeReiLrPH3w=
                  |        -----END CERTIFICATE----
                  |kind: v1
                  |        """.stripMargin

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
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("multiline-literal-3-keep-lines") {

    val yaml = s"""certificate: |+
                  |        -----BEGIN CERTIFICATE-----
                  |        0MTk0MVoXDenkKThvP7IS9q
                  |        +Dzv5hG392KWh5f8xJNs4LbZyl901MeReiLrPH3w=
                  |        -----END CERTIFICATE----
                  |
                  |
                  |kind: v1
                  |        """.stripMargin

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
    assertEventsEquals(yaml.events, expectedEvents)
  }

  test("multiline-literal-4") {
    val yaml = s"""key:
                  |  - content: |
                  |     [Unit]
                  |  - content: |
                  |     set -x
                  | 
                  |     """.stripMargin

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
    assertEventsEquals(yaml.events, expectedEvents)
  }
