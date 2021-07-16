package org.virtuslab.internal.load.parse

import com.eed3si9n.expecty.Expecty.expect
import org.virtuslab.internal.load.reader.YamlReader
import org.virtuslab.internal.load.parse.Event._

class SequenceSpec extends munit.FunSuite {

  test("should parse mapping with sequence value") {
    val yaml =
      s"""command:
         |    - /bin/sh
         |    - -c
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
        Scalar("/bin/sh"),
        Scalar("-c"),
        SequenceEnd,
        MappingEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

  test("should parse sequence of sequences") {
    val yaml =
      s"""-
         |  - v1
         |  - v2
         |-
         |  - v3
         |  - v4
         |""".stripMargin

    val reader = YamlReader(yaml)
    val events = ParserImpl.getEvents(reader)

    val expectedEvents = Right(
      List(
        StreamStart,
        DocumentStart(),
        SequenceStart,
        SequenceStart,
        Scalar("v1"),
        Scalar("v2"),
        SequenceEnd,
        SequenceStart,
        Scalar("v3"),
        Scalar("v4"),
        SequenceEnd,
        SequenceEnd,
        DocumentEnd(),
        StreamEnd
      )
    )
    expect(events == expectedEvents)
  }

}
