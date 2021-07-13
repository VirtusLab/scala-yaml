package org.virtuslab.internal.load

import munit.Clue.generate
import org.virtuslab.internal.YamlError
import org.virtuslab.internal.load.construct.Construct
import org.virtuslab.internal.load.compose.ComposerImpl
import org.virtuslab.yaml.Util.*

class EncoderSuite extends munit.FunSuite:

  case class Person(name: String, age: Int) derives Construct

  test("derives yaml encoder") {
    val yamlString =
      """age: 40
        |name: Mark McGwire
        |""".stripMargin

    assertEquals(yamlString.as[Person], Right(Person("Mark McGwire", 40)))
  }
