package org.virtuslab.yaml.decoder

import org.virtuslab.yaml.Node.ScalarNode
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
    case class OptionTypes(double: Option[Double], float: Option[Float], int: Option[Int], short: Option[Short])
        derives YamlDecoder

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
      5.5      -> 55.55
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
