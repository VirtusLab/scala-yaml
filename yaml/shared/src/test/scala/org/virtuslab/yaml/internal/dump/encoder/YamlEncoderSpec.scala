package org.virtuslab.yaml.internal.dump.encoder

import org.virtuslab.yaml.*

class YamlEncoderSpec extends munit.FunSuite:

  test("should deserialzie plain value") {
    val data: String = "aezakmi"
    val expected =
      s"""aezakmi
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("should derive encoder & deserialize case class (mapping)") {
    case class Stats(hr: Int, avg: Double, rbi: Int) derives YamlCodec
    val data = Stats(1, 1.0, 1)
    val expected =
      s"""hr: 1
         |avg: 1.0
         |rbi: 1
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("should deserialize sequence") {
    val data = Seq("Mark McGwire", "Sammy Sosa", "Ken Griffey")
    val expected =
      s"""- Mark McGwire
         |- Sammy Sosa
         |- Ken Griffey
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("should deserialize sequence of mappings") {
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

  test("should deserialize sequence of sequences") {
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

  test("should deserialize mapping of sequences") {
    case class Data(ints: Seq[Int], doubles: Seq[Double]) derives YamlCodec
    val data = Data(Seq(1, 2), Seq(3.0, 4.0))

    val expected =
      s"""ints: 
         |  - 1
         |  - 2
         |doubles: 
         |  - 3.0
         |  - 4.0
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("should deserialize mapping of mappings") {
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

  test("should deserialize map of [String, Char]") {
    val data = Map("1" -> 'a', "2" -> 'b', "3" -> 'c')
    val expected =
      s"""1: a
         |2: b
         |3: c
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("should deserialize set of Boolean") {
    val data = Set(true, false)
    val expected =
      s"""- true
         |- false
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("should derive encoder & deserialize enum cases") {
    enum SomeEnum derives YamlCodec:
      case Foo(value: Int)
      case Bar(price: Double)
    val data     = SomeEnum.Foo(1)
    val expected = "value: 1"

    assertEquals(data.asYaml.trim, expected)
  }

  test("should deserialize nested case classes") {
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

  test("should deserialize complex kubernetes mapping") {
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
