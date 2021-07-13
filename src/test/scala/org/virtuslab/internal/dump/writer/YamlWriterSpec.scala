package org.virtuslab.internal.dump.writer

import org.virtuslab.internal.dump.YamlWriter
import org.virtuslab.internal.dump.YamlWriter.*
import org.virtuslab.internal.YamlError
import com.eed3si9n.expecty.Expecty.expect

class YamlWriterSpec extends munit.FunSuite:

  case class Stats(hr: Int, avg: Double, rbi: Int) derives YamlWriter

  enum SomeEnum derives YamlWriter:
    case Foo(value: Int)
    case Bar(price: Double)

  test("derive writer for case class") {
    val stats = Stats(1, 1.0, 1)
    val expected =
      s"""|hr: 1
         |avg: 1.0
         |rbi: 1""".stripMargin

    expect(stats.toYaml() == expected)
  }

  test("derive writer for sequences of string") {
    val stats = Seq("Mark McGwire", "Sammy Sosa", "Ken Griffey")
    val expected =
      s"""- Mark McGwire
         |- Sammy Sosa
         |- Ken Griffey
         |""".stripMargin

    expect(stats.toYaml() == expected)
  }

  test("derive writer for enum") {
    val stats = SomeEnum.Foo(1)
    val expected =
      s"value: 1"

    val yaml = stats.toYaml()

    expect(yaml == expected)
  }
