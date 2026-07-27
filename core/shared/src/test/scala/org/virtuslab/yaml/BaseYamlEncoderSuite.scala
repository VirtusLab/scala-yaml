package org.virtuslab.yaml

class BaseYamlEncoderSuite extends munit.FunSuite {
  val newline = System.lineSeparator()

  test("plain value") {
    val data: String = "aezakmi"
    val expected =
      s"""aezakmi
         |""".stripMargin
    assertEquals(data.asYaml, expected)
  }

  test("sequence of sequences") {
    val data = Seq(
      Seq(1, 2),
      Seq(3, 4)
    )
    val expected =
      s"""-
         |  - 1
         |  - 2
         |-
         |  - 3
         |  - 4
         |""".stripMargin
    assertEquals(data.asYaml, expected)
  }
  test("strings requiring double quoting - special prefix/suffix & edge cases") {
    val data = Seq(
      "",   // length 0
      "-",  // length 1
      "?",  // length 1
      ":",  // length 1
      "- ", // starts with - and space
      "? ", // starts with ? and space
      ": ", // starts with : and space
      " a", // starts with whitespace
      "a ", // ends with whitespace
      ",a",
      "[a",
      "]a",
      "{a",
      "}a",
      "#a",
      "&a",
      "*a",
      "!a",
      "|a",
      ">a",
      "'a",
      "\"a",
      "%a",
      "@a",
      "`a"
    )
    val expected =
      s"""- ""
         |- "-"
         |- "?"
         |- ":"
         |- "- "
         |- "? "
         |- ": "
         |- " a"
         |- "a "
         |- ",a"
         |- "[a"
         |- "]a"
         |- "{a"
         |- "}a"
         |- "#a"
         |- "&a"
         |- "*a"
         |- "!a"
         |- "|a"
         |- ">a"
         |- "'a"
         |- "\\"a"
         |- "%a"
         |- "@a"
         |- "`a"
         |""".stripMargin
    assertEquals(data.asYaml, expected)
  }

  test("strings requiring double quoting - special substrings & control characters") {
    val data = Seq(
      " a",       // starts from " "
      "a:",       // trailing colon
      "true ",    // trailing whitespace
      "a: b",     // contains ": "
      "a # b",    // contains " #"
      "a\u001fb", // contains c < 32
      "a\u007fb"  // contains c == 127
    )
    val expected =
      s"""- " a"
         |- "a:"
         |- "true "
         |- "a: b"
         |- "a # b"
         |- "a\\u001fb"
         |- "a\\u007fb"
         |""".stripMargin
    assertEquals(data.asYaml, expected)
  }

  test("strings requiring double quoting - yaml patterns (booleans, numbers, nulls)") {
    val data = Seq(
      "null",
      "Null",
      "~",
      "false",
      "False",
      "true",
      "True",
      "-1",
      "-1.1",
      "-.inf",
      "+1",
      "+1.1",
      "+.inf",
      "0",
      "0o7",
      "0x1A",
      "123",
      "1.23",
      ".NaN",
      ".inf"
    )
    val expected =
      s"""- "null"
         |- "Null"
         |- "~"
         |- "false"
         |- "False"
         |- "true"
         |- "True"
         |- "-1"
         |- "-1.1"
         |- "-.inf"
         |- "+1"
         |- "+1.1"
         |- "+.inf"
         |- "0"
         |- "0o7"
         |- "0x1A"
         |- "123"
         |- "1.23"
         |- ".NaN"
         |- ".inf"
         |""".stripMargin
    assertEquals(data.asYaml, expected)
  }

  test("escape characters in double quoted strings") {
    // String starts with space to enforce double quoting logic fallback,
    // thereby hitting the escapeDoubleQuoted switch branch exactly.
    val data = Seq(" \n\r\t\b\f\"\\")
    val expected =
      s"""- " \\n\\r\\t\\b\\f\\"\\\\"
         |""".stripMargin
    assertEquals(data.asYaml, expected)
  }

  test("sequence") {
    val data = Seq("Mark McGwire", "Sammy Sosa", "Ken Griffey")
    val expected =
      s"""- Mark McGwire
         |- Sammy Sosa
         |- Ken Griffey
         |""".stripMargin
    assertEquals(data.asYaml, expected)
  }

  test("map") {
    val data = Map("1" -> 'a', "2" -> 'b', "3" -> 'c')
    val expected =
      s""""1": a
         |"2": b
         |"3": c
         |""".stripMargin
    assertEquals(data.asYaml, expected)
  }

  test("set of boolean") {
    val data = Set(true, false)
    val expected =
      s"""- true
         |- false
         |""".stripMargin
    assertEquals(data.asYaml, expected)
  }

  test("encoding of non-printable characters") {
    // yaml ends with newline
    assertEquals(Char.MinValue.toString.asYaml, "\"\\u0000\"\n")
    assertEquals('\u001f'.toString.asYaml, "\"\\u001f\"\n")
    assertEquals('\u0080'.toString.asYaml, "\"\\u0080\"\n")
    assertEquals('\u009f'.toString.asYaml, "\"\\u009f\"\n")
    assertEquals(Char.MaxValue.toString.asYaml, "\"\\uffff\"\n")
  }
}
