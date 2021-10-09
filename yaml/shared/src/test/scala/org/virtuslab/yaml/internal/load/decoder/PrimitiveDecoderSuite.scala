package org.virtuslab.yaml.internal.load.decoder

import org.virtuslab.yaml.*

class PrimitiveDecoderSuite extends munit.FunSuite:

  test("derive construct for primitive number type") {

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

  test("derive construct for option") {
    // todo Option YamlEncoder
    case class OptionTypes(double: Option[Double], float: Option[Float], int: Option[Int])
        derives YamlDecoder

    val numberYaml =
      s"""double: ${Double.MaxValue}
         |float: null
         |int: 
         |""".stripMargin

    val expectedNumber = OptionTypes(
      double = Some(Double.MaxValue),
      float = None,
      int = None
    )

    assertEquals(numberYaml.as[OptionTypes], Right(expectedNumber))
  }

  test("derive construct for sequence".ignore) {

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

  test("derive construct for mapping") {

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

  test("derive construct for mapping".ignore) {
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

  test("derive construct for nested case class") {

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

  test("derive construct for sequence of mappings") {

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
