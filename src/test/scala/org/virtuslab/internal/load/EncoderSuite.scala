package org.virtuslab.internal.load

import org.virtuslab.Yaml.*

class EncoderSuite extends munit.FunSuite:

  case class Person(name: String, age: Int) derives YamlDecoder

  test("derives yaml encoder") {
    val yamlString =
      """age: 40
        |name: Mark McGwire
        |""".stripMargin

    assertEquals(yamlString.as[Person], Right(Person("Mark McGwire", 40)))
  }
