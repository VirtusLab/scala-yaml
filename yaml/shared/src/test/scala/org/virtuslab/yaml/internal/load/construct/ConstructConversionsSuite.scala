package org.virtuslab.yaml.internal.load.construct

import org.virtuslab.yaml.*
import org.virtuslab.yaml.Node.*
import org.virtuslab.yaml.syntax.YamlPrimitive
import scala.language.implicitConversions

class ConstructConversionsSuite extends munit.FunSuite:

  case class DummyClass(key: String, value: String) derives YamlCodec

  test("throw errors due to forbidden mapping - key must be scalar node") {
    val node = MappingNode(
      MappingNode(
        "key" -> "value"
      ) -> MappingNode(
        "key2" -> "value2"
      )
    )

    val expectedConstructError =
      Left(ConstructError(s"Parameter of a class must be a scalar value"))

    assertEquals(node.as[DummyClass], expectedConstructError)
  }
