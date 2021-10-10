package org.virtuslab.yaml.internal.load.decoder

import org.virtuslab.yaml.*
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.Node.SequenceNode
import org.virtuslab.yaml.Node.ScalarNode

class DecoderErrorsSuite extends BaseDecoderErrorSuite:

  case class Person(name: String, age: Int) derives YamlCodec

  test("fail-for-invalid-int") {

    val yaml = """|name: John
                  |age: xxx
                  |""".stripMargin

    assertError(
      yaml.as[Person],
      s"""|For input string: "xxx"
          |at 1:5, expected Int
          |age: xxx
          |     ^
          |""".stripMargin
    )
  }
end DecoderErrorsSuite
