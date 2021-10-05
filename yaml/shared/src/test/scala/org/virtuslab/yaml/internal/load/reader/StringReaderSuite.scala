package org.virtuslab.yaml.internal.load.reader

import org.virtuslab.yaml.*

class StringReaderSuite extends munit.FunSuite:

  test("String reader counts positions correctly") {
    val input = s"""|a
                    |b${"\r"}
                    |c
                    |""".stripMargin

    val lines  = input.split("\n").toVector
    val reader = StringReader(input)
    assertEquals(reader.pos(), Position(0, 0, 0, lines))
    assertEquals(reader.read(), 'a')
    assertEquals(reader.pos(), Position(1, 0, 1, lines))
    reader.skipCharacter()
    assertEquals(reader.pos(), Position(2, 1, 0, lines))
    assertEquals(reader.read(), 'b')
    reader.skipCharacter()
    assertEquals(reader.pos(), Position(5, 2, 0, lines))
    assertEquals(reader.read(), 'c')
  }
