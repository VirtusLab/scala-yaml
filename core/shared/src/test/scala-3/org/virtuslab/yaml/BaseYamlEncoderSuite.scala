package org.virtuslab.yaml

import org.virtuslab.yaml.*

class YamlEncoderSuite extends munit.FunSuite:

  test("plain value") {
    val data: String = "aezakmi"
    val expected =
      s"""aezakmi
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

  test("sequence of mappings") {
    case class Data(int: Int, double: Double) derives YamlCodec
    val data = Seq(
      Data(1, 1.997),
      Data(2, 2.997)
    )

    val expected =
      s"""- 
         |  int: 1
         |  double: 1.997
         |- 
         |  int: 2
         |  double: 2.997
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

  test("mapping of mappings") {
    case class Nested(a: Int, b: String) derives YamlCodec
    case class Data(first: Nested, second: Nested) derives YamlCodec
    val data = Data(Nested(1, "one"), Nested(2, "two"))

    val expected =
      s"""first: 
         |  a: 1
         |  b: one
         |second: 
         |  a: 2
         |  b: two
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("map") {
    val data = Map("1" -> 'a', "2" -> 'b', "3" -> 'c')
    val expected =
      s"""1: a
         |2: b
         |3: c
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

  test("enum case") {
    enum SomeEnum derives YamlCodec:
      case Foo(value: Int)
      case Bar(price: Double)
    val data     = SomeEnum.Foo(1)
    val expected = "value: 1"

    assertEquals(data.asYaml.trim, expected)
  }

  test("nested case classes") {
    case class Address(city: String) derives YamlCodec
    case class Person(address: Address, ints: Seq[Int]) derives YamlCodec

    val data = Person(Address("Anytown"), Seq(1, 2))
    val expected =
      s"""address: 
         |  city: Anytown
         |ints: 
         |  - 1
         |  - 2
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("option") {
    case class Foo(field: Option[String]) derives YamlCodec

    val some = Foo(Some("some"))
    val none = Foo(None)

    assertNoDiff(some.asYaml, "field: some")
    assertNoDiff(none.asYaml, "field: !!null")
  }

  test("complex kubernetes mapping") {
    case class Web(build: String, ports: List[String], volumes: List[String]) derives YamlCodec
    case class Redis(image: String) derives YamlCodec
    case class Services(web: Web, redis: Redis) derives YamlCodec
    case class Compose(version: String, services: Services) derives YamlCodec

    val data = Compose(
      version = "3.9",
      services = Services(
        web = Web(
          build = ".",
          ports = List("5000:5000"),
          volumes = List(".:/code", "logvolume01:/var/log")
        ),
        redis = Redis(
          image = "redis:alpine"
        )
      )
    )
    val expected = """version: 3.9
                     |services: 
                     |  web: 
                     |    build: .
                     |    ports: 
                     |      - 5000:5000
                     |    volumes: 
                     |      - .:/code
                     |      - logvolume01:/var/log
                     |  redis: 
                     |    image: redis:alpine
                     |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("encoding of non-printable characters") {
    // yaml ends with newline
    assertEquals(Char.MinValue.toString.asYaml, "\\u0000\n")
    assertEquals(Char.MaxValue.toString.asYaml, "\\uFFFF\n")
  }
