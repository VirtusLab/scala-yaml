package org.virtuslab.yaml

import scala.language.implicitConversions

import org.virtuslab.yaml.*
import org.virtuslab.yaml.syntax.YamlPrimitive

class ConstructSuite extends munit.FunSuite:
  import Node.*
  case class Stats(hr: Int, avg: Double, rbi: Int) derives YamlCodec

  enum SomeEnum derives YamlCodec:
    case Foo(value: Int)
    case Bar(price: Double)

  test("derive construct for case class") {
    val node = MappingNode(
      ScalarNode("hr")  -> ScalarNode("65"),
      ScalarNode("avg") -> ScalarNode("0.278"),
      ScalarNode("rbi") -> ScalarNode("147")
    )
    val expected = Right(Stats(65, 0.278, 147))
    assertEquals(node.as[Stats], expected)
  }

  test("derive construct for sealed trait") {
    val foo = MappingNode(ScalarNode("value") -> ScalarNode("65"))
    assertEquals(foo.as[SomeEnum], Right(SomeEnum.Foo(65)))

    val bar = MappingNode(ScalarNode("price") -> ScalarNode("65.997"))
    assertEquals(bar.as[SomeEnum], Right(SomeEnum.Bar(65.997)))
  }

  test("key must be scalar node") {
    case class DummyClass(key: String, value: String) derives YamlCodec
    val node = MappingNode(
      MappingNode(
        "key" -> "value"
      ) -> MappingNode(
        "key2" -> "value2"
      )
    )

    val expectedConstructError =
      Left(
        ConstructError.from(
          s"Parameter of a class must be a scalar value",
          MappingNode("key" -> "value")
        )
      )
    assertEquals(node.as[DummyClass], expectedConstructError)
  }

  test("decode as Any") {

    val node = MappingNode(
      SequenceNode(1, 2) -> ScalarNode("seq")
    )

    val expected = Map[Any, Any](
      Seq(1, 2) -> "seq"
    )

    assertEquals(node.as[Any], Right(expected))
  }
