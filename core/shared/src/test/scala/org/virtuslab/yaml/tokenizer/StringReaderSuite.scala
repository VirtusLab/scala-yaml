package org.virtuslab.yaml.tokenizer

import org.virtuslab.yaml._
import org.virtuslab.yaml.internal.load.reader.StringReader

class StringReaderSuite extends munit.FunSuite {

  test("count position") {
    val input = s"""|a
                    |b${"\r"}
                    |c
                    |""".stripMargin

    val lines  = input.split("\n", -1).toVector
    val reader = new StringReader(input)
    assertEquals(reader.range, Range(Position(0, 0, 0), lines))
    assertEquals(reader.read(), 'a')
    assertEquals(reader.range, Range(Position(1, 0, 1), lines))
    reader.skipCharacter()
    assertEquals(reader.range, Range(Position(2, 1, 0), lines))
    assertEquals(reader.read(), 'b')
    reader.skipCharacter()
    assertEquals(reader.range, Range(Position(5, 2, 0), lines))
    assertEquals(reader.read(), 'c')
  }

  test("parse planin value with empty lines") {
    val input = s"""|a
                    |
                    |
                    |""".stripMargin

    val lines  = input.split("\n", -1).toVector
    val reader = new StringReader(input)
    assertEquals(reader.range, Range(Position(0, 0, 0), lines))
    assertEquals(reader.read(), 'a')
    assertEquals(reader.range, Range(Position(1, 0, 1), lines))
    assertEquals(reader.read(), '\n')
    assertEquals(reader.range.errorMsg, "\n^")
  }
}
