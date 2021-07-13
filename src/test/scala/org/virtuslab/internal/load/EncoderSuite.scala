package org.virtuslab.internal.load

import org.virtuslab.internal.YamlError
import org.virtuslab.internal.load.construct.Construct
import org.virtuslab.yaml.YamlDecoder

class EncoderSuite extends munit.FunSuite:

  case class Person(name: String, age: Int) derives YamlDecoder

  extension (str: String)
    def as[T](using decoder: YamlDecoder[T]): Either[YamlError, T] = decoder.from(str)

  test("derives yaml encoder") {
    val yamlString =
      """age: 40
        |name: Mark McGwire
        |""".stripMargin

    assertEquals(yamlString.as[Person], Right(Person("Mark McGwire", 40)))
  }
