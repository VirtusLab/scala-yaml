package org.virtuslab.yaml.internal.load.construct

import org.virtuslab.yaml.*

class ConstructSuite extends munit.FunSuite:
  import Node.*
  case class Stats(hr: Int, avg: Double, rbi: Int) derives YamlCodec

  enum SomeEnum derives YamlCodec:
    case Foo(value: Int)
    case Bar(price: Double)

  test("derive construct for case class") {
    val node = MappingNode(
      "hr"  -> ScalarNode("65"),
      "avg" -> ScalarNode("0.278"),
      "rbi" -> ScalarNode("147")
    )
    val expected = Right(Stats(65, 0.278, 147))
    assertEquals(node.as[Stats], expected)
  }

  test("derive construct for sealed trait") {
    val foo = MappingNode("value" -> ScalarNode("65"))
    assertEquals(foo.as[SomeEnum], Right(SomeEnum.Foo(65)))

    val bar = MappingNode("price" -> ScalarNode("65.997"))
    assertEquals(bar.as[SomeEnum], Right(SomeEnum.Bar(65.997)))
  }
