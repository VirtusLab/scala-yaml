package org.virtuslab.internal.dump.writer

import org.virtuslab.Yaml.*
import com.eed3si9n.expecty.Expecty.expect

class YamlWriterSpec extends munit.FunSuite:

  case class Stats(hr: Int, avg: Double, rbi: Int) derives YamlEncoder

  enum SomeEnum derives YamlEncoder:
    case Foo(value: Int)
    case Bar(price: Double)

  test("should derive encoder & deserialize case class") {
    val stats = Stats(1, 1.0, 1)
    val expected =
      s"""|hr: 1
         |avg: 1.0
         |rbi: 1""".stripMargin

    expect(stats.asYaml == expected)
  }

  test("should deserialize seq of string") {
    val stats = Seq("Mark McGwire", "Sammy Sosa", "Ken Griffey")
    val expected =
      s"""- Mark McGwire
         |- Sammy Sosa
         |- Ken Griffey
         |""".stripMargin

    expect(stats.asYaml == expected)
  }

  test("should deserialize seq of int") {
    val stats = Seq(1, 2, 3)
    val expected =
      s"""- 1
         |- 2
         |- 3
         |""".stripMargin

    expect(stats.asYaml == expected)
  }

  test("should deserialize seq of double") {
    val stats = Seq(1.53, 2.27, 3.33)
    val expected =
      s"""- 1.53
         |- 2.27
         |- 3.33
         |""".stripMargin

    expect(stats.asYaml == expected)
  }

  test("should derive encoder & deserialize enum cases") {
    val stats = SomeEnum.Foo(1)
    val expected =
      s"value: 1"

    val yaml = stats.asYaml

    expect(yaml == expected)
  }
