package org.virtuslab.internal.load.construct

import org.virtuslab.internal.load.construct.Construct
import org.virtuslab.internal.load.compose.Node.*
import org.virtuslab.internal.load.compose.Node
import org.virtuslab.internal.YamlError

class ConstructSuite extends munit.FunSuite:

  case class Stats(hr: Int, avg: Double, rbi: Int) derives Construct

  enum SomeEnum derives Construct:
    case Foo(value: Int)
    case Bar(price: Double)

  extension (node: Node) def as[T](using c: Construct[T]): Either[YamlError, T] = c.construct(node)

  test("derive construct for case class") {
    val node = MappingNode(
      KeyValueNode(ScalarNode("hr"), ScalarNode("65")),
      KeyValueNode(ScalarNode("avg"), ScalarNode("0.278")),
      KeyValueNode(ScalarNode("rbi"), ScalarNode("147"))
    )
    val expected = Right(Stats(65, 0.278, 147))
    assertEquals(node.as[Stats], expected)
  }

  test("derive construct for sealed trait") {
    val foo = MappingNode(
      KeyValueNode(ScalarNode("value"), ScalarNode("65"))
    )
    assertEquals(foo.as[SomeEnum], Right(SomeEnum.Foo(65)))

    val bar = MappingNode(
      KeyValueNode(ScalarNode("price"), ScalarNode("65.997"))
    )
    assertEquals(bar.as[SomeEnum], Right(SomeEnum.Bar(65.997)))
  }
