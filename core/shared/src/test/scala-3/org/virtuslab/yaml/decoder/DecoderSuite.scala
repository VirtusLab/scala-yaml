package org.virtuslab.yaml.decoder

import org.virtuslab.yaml.Node.*
import org.virtuslab.yaml.*

class DecoderSuite extends munit.FunSuite:

  test("numbers") {

    case class ValueTypes(
        double: Double,
        float: Float,
        long: Long,
        int: Int,
        short: Short,
        byte: Byte,
        boolean: Boolean
    ) derives YamlCodec

    val numberYaml =
      s"""double: ${Double.MaxValue}
         |float:  ${Float.MinValue}
         |long:   ${Long.MaxValue}
         |int:    ${Int.MinValue}
         |short:  ${Short.MaxValue}
         |byte:   ${Byte.MinValue}
         |boolean: false
         |""".stripMargin

    val expectedNumber = ValueTypes(
      double = Double.MaxValue,
      float = Float.MinValue,
      long = Long.MaxValue,
      int = Int.MinValue,
      short = Short.MaxValue,
      byte = Byte.MinValue,
      boolean = false
    )

    assertEquals(numberYaml.as[ValueTypes], Right(expectedNumber))
  }

  test("option") {
    // todo Option YamlEncoder
    case class OptionTypes(
        double: Option[Double],
        float: Option[Float],
        int: Option[Int],
        short: Option[Short]
    ) derives YamlDecoder

    val numberYaml =
      s"""double: ${Double.MaxValue}
         |float: null
         |int: 
         |""".stripMargin

    val expectedNumber = OptionTypes(
      double = Some(Double.MaxValue),
      float = None,
      int = None,
      short = None
    )

    assertEquals(numberYaml.as[OptionTypes], Right(expectedNumber))
  }

  test("sequence") {

    case class SequenceTypes(doubles: List[Double], floats: Seq[Float], ints: Set[Int])
        derives YamlCodec

    val flowSequenceYaml =
      s"""doubles: ['1.0', '2.0']
         |floats: ['1.0', '2.0']
         |ints: ['1', '2']
         |""".stripMargin

    val sequenceYaml =
      s"""doubles:
         |  - 1.0
         |  - 2.0
         |floats:
         |  - 1.0
         |  - 2.0
         |ints: 
         |  - 1 
         |  - 2
         |""".stripMargin

    val expectedSequence = SequenceTypes(
      doubles = List(1.0, 2.0),
      floats = Seq(1.0.toFloat, 2.0.toFloat),
      ints = Set(1, 2)
    )

    assertEquals(flowSequenceYaml.as[SequenceTypes], Right(expectedSequence))
    assertEquals(sequenceYaml.as[SequenceTypes], Right(expectedSequence))
  }

  test("mapping") {

    case class SequenceTypes(doubles: Map[String, Double], floats: Map[String, List[Float]])
        derives YamlCodec

    val mappingYaml =
      s"""doubles:
         |  double1: 1
         |  double2: 2
         |floats: 
         |  floats: 
         |    - 3.0
         |    - 4.0
         |""".stripMargin

    val expectedMapping = SequenceTypes(
      doubles = Map("double1" -> 1.0, "double2" -> 2.0),
      floats = Map("floats" -> List(3.0.toFloat, 4.0.toFloat))
    )

    assertEquals(mappingYaml.as[SequenceTypes], Right(expectedMapping))
  }

  test("mapping-2") {
    case class SequenceTypes(doubles: Map[String, Double], floats: Map[String, List[Float]])
        derives YamlCodec

    val flowMappingYaml =
      s"""doubles: { double1: 1.0, double2: 2.0 }
         |floats: 
         |  floats: [ '3.0', '4.0' ]
         |""".stripMargin

    val expectedMapping = SequenceTypes(
      doubles = Map("double1" -> 1.0, "double2" -> 2.0),
      floats = Map("floats" -> List(3.0.toFloat, 4.0.toFloat))
    )

    assertEquals(flowMappingYaml.as[SequenceTypes], Right(expectedMapping))
  }

  test("case class") {

    case class Port(port: Int) derives YamlCodec
    case class Selector(app: String, tier: String) derives YamlCodec
    case class Spec(`type`: String, ports: List[Port], selector: Selector) derives YamlCodec
    case class Config(spec: Spec) derives YamlCodec

    val yaml =
      s"""spec:
         |  # comment
         |  type: NodePort
         |  # to delete
         |  ports:
         |  - port: 80
         |  selector:
         |    app: guestbook
         |    tier: frontend
         |""".stripMargin

    val expectedConfig = Config(
      spec = Spec(
        `type` = "NodePort",
        ports = List(Port(80)),
        selector = Selector("guestbook", "frontend")
      )
    )
    assertEquals(yaml.as[Config], Right(expectedConfig))
  }

  test("sequence of mappings") {

    sealed trait Address derives YamlCodec
    case class Network(network: String, port: Int) extends Address
    case class Local(local: String, port: Int)     extends Address

    case class Spec(addresses: List[Address]) derives YamlCodec

    val yaml =
      s"""|addresses:
          |  - port: 80
          |    local: localhost
          |  - port: 81
          |    network: 127.0.0.1
          |""".stripMargin

    val expectedSpec = Spec(
      addresses = List(Local("localhost", 80), Network("127.0.0.1", 81))
    )

    assertEquals(yaml.as[Spec], Right(expectedSpec))
  }

  test("codec mapping") {
    case class Capital(value: String)

    object Capital:
      given YamlCodec[Capital] =
        YamlCodec.make[String].mapInvariant(s => Capital(s.toUpperCase))(_.value.toUpperCase)

    assertEquals(
      "- hello\n- world\n".as[List[Capital]],
      Right(List(Capital("HELLO"), Capital("WORLD")))
    )
    assertEquals(List(Capital("hello"), Capital("world")).asYaml, "- HELLO\n- WORLD\n")
  }

  test("alias for scalar node") {
    val yaml =
      s"""|- &a 5
          |- *a
          |""".stripMargin

    assertEquals(yaml.as[Any], Right(List(5, 5)))
  }

  test("alias for sequence node") {
    val yaml =
      s"""|seq1: &a 
          | - 1
          | - 2
          |seq2: *a
          |""".stripMargin

    assertEquals(
      yaml.as[Any],
      Right(
        Map(
          "seq1" -> List(1, 2),
          "seq2" -> List(1, 2)
        )
      )
    )
  }

  test("alias for value in sequence") {
    val yaml =
      s"""|- &b
          |  name: Mark McGwire
          |  hr:   65
          |- *b
          |""".stripMargin

    assertEquals(
      yaml.as[Any],
      Right(
        List(
          Map("name" -> "Mark McGwire", "hr" -> 65),
          Map("name" -> "Mark McGwire", "hr" -> 65)
        )
      )
    )
  }

  test("alias for flow sequence node") {
    val yaml =
      s"""|seq1: &a [1, 2]
          |seq2: *a
          |""".stripMargin

    assertEquals(
      yaml.as[Any],
      Right(
        Map(
          "seq1" -> List(1, 2),
          "seq2" -> List(1, 2)
        )
      )
    )
  }

  test("alias for mapping node") {
    val yaml =
      s"""|map1: &a 
          |  1: 2  
          |  k1: v1
          |map2: *a
          |""".stripMargin

    assertEquals(
      yaml.as[Any],
      Right(
        Map(
          "map1" -> Map(1 -> 2, "k1" -> "v1"),
          "map2" -> Map(1 -> 2, "k1" -> "v1")
        )
      )
    )
  }

  test("alias for flow mapping node") {
    val yaml =
      s"""|map1: &a {
          |  1: 2,  
          |  k1: v1
          |}
          |map2: *a
          |""".stripMargin

    assertEquals(
      yaml.as[Any],
      Right(
        Map(
          "map1" -> Map(1 -> 2, "k1" -> "v1"),
          "map2" -> Map(1 -> 2, "k1" -> "v1")
        )
      )
    )
  }

  test("decode into Map[Any, Any]") {

    val yaml =
      s"""|123: 321
          |string: aezakmi
          |true: false
          |5.5: 55.55
          |""".stripMargin

    val expected = Map[Any, Any](
      123      -> 321,
      "string" -> "aezakmi",
      true     -> false,
      5.5f     -> 55.55f
    )

    assertEquals(yaml.as[Map[Any, Any]], Right(expected))
  }

  test("decode using custom tag") {
    case class Custom(x: Int, doubledX: Int)

    val yaml =
      s"""|!Custom 5
          |""".stripMargin

    val expected = Custom(5, 10)

    val decoder = YamlDecoder[Custom] { case ScalarNode(value, _) =>
      val int = value.toInt
      Right(Custom(int, int * 2))
    }

    given settings: LoadSettings = LoadSettings(
      Map(CustomTag("!Custom") -> decoder)
    )

    assertEquals(yaml.as[Any], Right(expected))
  }

  test("option") {
    case class Foo(int: Int, string: String) derives YamlCodec

    val foo =
      """|- int: 1
         |  string: "1"
         |- !!null
         |""".stripMargin.as[List[Option[Foo]]]

    assertEquals(foo, Right(List(Some(Foo(1, "1")), None)))
  }

  test("issue 222 - parse edge cases of booleans floats doubles and integers") {
    case class Data(
        booleans: List[Boolean],
        integers: List[Int],
        floats: List[Float],
        `also floats`: List[Float],
        `also doubles`: List[Double]
    ) derives YamlCodec

    val yaml = """booleans: [ true, True, false, FALSE ]
                 |integers: [ 0, 0o7, 0x3A, -19 ]
                 |floats: [
                 |  0., -0.0, .5, +12e03, -2E+05 ]
                 |also floats: [
                 |  .inf, -.Inf, +.INF, .NAN, .nan, .NaN]
                 |also doubles: [
                 |  .inf, -.Inf, +.INF, .NAN, .nan, .NaN]""".stripMargin

    val expected = Data(
      booleans = List(true, true, false, false),
      integers = List(0, 7, 58, -19),
      floats = List(0.0f, -0.0f, 0.5f, 12000.0f, -200000.0f),
      `also floats` = List(
        Float.PositiveInfinity,
        Float.NegativeInfinity,
        Float.PositiveInfinity,
        Float.NaN,
        Float.NaN,
        Float.NaN
      ),
      `also doubles` = List(
        Double.PositiveInfinity,
        Double.NegativeInfinity,
        Double.PositiveInfinity,
        Double.NaN,
        Double.NaN,
        Double.NaN
      )
    )

    yaml.as[Data] match
      case Left(error: YamlError) => throw error
      case Right(data) =>
        assertEquals(data.booleans, expected.booleans)
        assertEquals(data.integers, expected.integers)
        assertEquals(data.floats, expected.floats)
        data.`also floats`.zipAll(expected.`also floats`, 0f, 0f).foreach { case (a, b) =>
          assertEqualsFloat(a, b, 0f)
        }
        data.`also doubles`.zipAll(expected.`also doubles`, 0.0d, 0.0d).foreach { case (a, b) =>
          assertEqualsDouble(a, b, 0.0d)
        }
  }

  test("issue 281 - parse multiline string") {
    case class Data(description: String) derives YamlCodec

    val yaml = """|description: |-
                  |  Hi
                  |  my name
                  |  is John""".stripMargin

    val expectedStr = """Hi
                        |my name
                        |is John""".stripMargin

    yaml.as[Data] match
      case Left(error: YamlError) =>
        fail(s"failed with YamlError: $error")
      case Right(data) =>
        assertEquals(data.description, expectedStr)
  }

  test("issue 258 - scala-yaml should not escape backslashes in single-quoted strings") {
    raw"'hello\there'".as[String] match
      case Right(s) => assertEquals(s, raw"hello\there")
      case Left(e)  => fail(s"error ${e.msg}", e)
  }

  test("issue 86 - parsing key with empty value") {
    case class Foo(
        `single line`: Option[Int] = None,
        `multi line`: Option[Int] = None,
        a: String
    ) derives YamlCodec

    val yaml = """|---
                  |- { "single line", a: b}
                  |- { "multi
                  |  line", a: b}
                  |- { "single line": 23, a: c}
                  |- { "multi
                  |  line": 42, a: c}""".stripMargin

    yaml.as[List[Foo]] match
      case Left(error: YamlError) =>
        fail(s"failed with YamlError: $error")
      case Right(foos) =>
        assert(foos.size == 4)
        val foo1 = foos(0)
        val foo2 = foos(1)
        val foo3 = foos(2)
        val foo4 = foos(3)
        assertEquals(foo1.`single line`, None)
        assertEquals(foo1.`multi line`, None)
        assertEquals(foo1.a, "b")
        assertEquals(foo2.`single line`, None)
        assertEquals(foo2.`multi line`, None)
        assertEquals(foo2.a, "b")
        assertEquals(foo3.`single line`, Some(23))
        assertEquals(foo3.`multi line`, None)
        assertEquals(foo3.a, "c")
        assertEquals(foo4.`single line`, None)
        assertEquals(foo4.`multi line`, Some(42))
        assertEquals(foo4.a, "c")
  }

  test("issue 314 - decoding doubles as Any loses precision") {
    val yaml = "value: 0.018256052173961423"

    yaml.as[Any] match
      case Left(error: YamlError) =>
        fail(s"failed with YamlError: $error")
      case Right(value) =>
        assertEquals(value, Map("value" -> 0.018256052173961423))
  }

  test("issue 120 - fail conversion of !!null to non-optional types") {
    case class Foo(key1: Int, key2: Int) derives YamlDecoder

    val yaml =
      """|key1: 1
         |key2: !!null
         |""".stripMargin

    val bar = yaml.as[Foo]

    bar match
      case Left(error: YamlError) =>
        assert(error.msg.contains("Could't construct int from null (tag:yaml.org,2002:null)"))
      case Right(data) => fail(s"expected failure, but got: $data")
  }

  test("default parameters for case classes can be used when decoding") {
    case class Foo(a: Int = 1, b: String = "test", c: Option[Int] = None, d: Double)
        derives YamlCodec

    val yaml = """d: 1.0""".stripMargin

    yaml.as[Foo] match
      case Left(error: YamlError) =>
        fail(s"failed with YamlError: $error")
      case Right(foo) =>
        assertEquals(foo.a, 1)
        assertEquals(foo.b, "test")
        assertEquals(foo.c, None)
        assertEquals(foo.d, 1.0)
  }

  test("default parameters for case classes are evaluated lazily") {
    var times = 0
    def createB = {
      times += 1
      s"test-${times}"
    }
    case class Foo(a: Int, b: String = createB) derives YamlCodec

    val yaml = """a: 1""".stripMargin

    yaml.as[Foo] match
      case Left(error: YamlError) =>
        fail(s"failed with YamlError: $error")
      case Right(foo) =>
        assertEquals(foo.a, 1)
        assertEquals(foo.b, "test-1")

    yaml.as[Foo] // skip test-2

    yaml.as[Foo] match
      case Left(error: YamlError) =>
        fail(s"failed with YamlError: $error")
      case Right(foo) =>
        assertEquals(foo.a, 1)
        assertEquals(foo.b, "test-3")
  }

  test("default parameters are not evaluated when they are provided in yaml") {
    var evaluated = false
    def createB = {
      evaluated = true
      "default"
    }
    case class Foo(a: Int, b: String = createB) derives YamlCodec

    val yaml = """a: 1
                 |b: from yaml""".stripMargin

    yaml.as[Foo] match
      case Left(error: YamlError) =>
        fail(s"failed with YamlError: $error")
      case Right(foo) =>
        assertEquals(foo.a, 1)
        assertEquals(foo.b, "from yaml")
        assert(!evaluated)
  }

  test("Fails decoding -XXXinf as Float") {
    val yaml = "-XXXinf"

    yaml.as[Float] match
      case Left(e: ConstructError) =>
        assertEquals(e.expected, Some("Float"))
      case Left(e) =>
        fail(s"Should fail, but got $e", e)
      case Right(value) =>
        fail(s"Should fail, but got $value")
  }
