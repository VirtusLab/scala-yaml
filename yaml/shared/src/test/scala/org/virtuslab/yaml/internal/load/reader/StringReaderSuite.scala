package org.virtuslab.yaml.internal.load.reader

import org.virtuslab.yaml.*

class StringReaderSuite extends munit.FunSuite:

  test("String reader counts positions correctly") {
    val input = s"""|a
                    |b${"\r"}
                    |c
                    |""".stripMargin

    val reader = StringReader(input)
    assertEquals(reader.pos(), Position(0, 1, 1))
    assertEquals(reader.read(), 'a')
    assertEquals(reader.pos(), Position(1, 1, 2))
    reader.skipCharacter()
    assertEquals(reader.pos(), Position(2, 2, 1))
    assertEquals(reader.read(), 'b')
    reader.skipCharacter()
    assertEquals(reader.pos(), Position(5, 3, 1))
    assertEquals(reader.read(), 'c')
  }
