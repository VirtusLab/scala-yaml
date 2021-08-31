package org.virtuslab.yaml.internal.load.decoder

import org.virtuslab.yaml.*
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.Node.SequenceNode
import org.virtuslab.yaml.Node.ScalarNode

class DecoderErrorsSuite extends munit.FunSuite:

  test("should indicate invalid input and expected type") {
    case class Person(name: String, age: Int) derives YamlCodec

    val yaml = """|name: John
                  |age: xxx
                  |""".stripMargin

    val expected = s"""|For input string: "xxx"
                       |age: xxx
                       |     ^^^ at 2:6, expected Int""".stripMargin

    val maybePerson = yaml.as[Person]

    maybePerson match {
      case r @ Right(value)       => fail(s"Get $r, expected Left")
      case Left(error: YamlError) => assertNoDiff(error.msg, expected)
    }

  }
