import org.virtuslab.yaml.*
import org.virtuslab.yaml.Node.*
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class YamlEncoderSpec extends munit.FunSuite {
  test("sequence of mappings") {
    case class Data(int: Int, double: Double) derives YamlCodec

    val data = Seq(Data(1, 1.997), Data(2, 2.997))
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

  test("mapping of sequences") {
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

  test("enum case") {
    enum SomeEnum derives YamlCodec {
      case Foo(value: Int)
      case Bar(price: Double)
    }

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
    val expected =
      """version: "3.9"
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

  test("mapping of mappings (deep nesting)") {
    case class Data(map: Map[String, Map[String, String]]) derives YamlCodec

    val data = Data(Map("outer" -> Map("inner1" -> "val1", "inner2" -> "val2")))
    val expected =
      s"""map:
         |  outer:
         |    inner1: val1
         |    inner2: val2
         |""".stripMargin
    assertEquals(data.asYaml, expected)
  }

  test("sequence of mappings") {
    case class Data(seq: Seq[Map[String, String]]) derives YamlCodec

    val data = Data(Seq(Map("k1" -> "v1"), Map("k2" -> "v2")))
    val expected =
      s"""seq:
         |  -
         |    k1: v1
         |  -
         |    k2: v2
         |""".stripMargin
    assertEquals(data.asYaml, expected)
  }

  test("primitives serialization (plain styles)") {
    case class Primitives(b: Boolean, i: Int, f: Double) derives YamlCodec

    val data = Primitives(true, 42, 3.14)
    val expected =
      s"""b: true
         |i: 42
         |f: 3.14
         |""".stripMargin
    assertEquals(data.asYaml, expected)
  }
}
