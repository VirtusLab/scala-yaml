package org.virtuslab.internal.load.parse

import com.eed3si9n.expecty.Expecty.expect
import org.virtuslab.internal.load.reader.YamlReader
import org.virtuslab.internal.load.parse.Event._

class SequenceSpec extends munit.FunSuite {

  test("should parse host:port as scalar") {
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

}
