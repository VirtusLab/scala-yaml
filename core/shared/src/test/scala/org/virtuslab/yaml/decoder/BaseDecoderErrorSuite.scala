package org.virtuslab.yaml.decoder

import org.virtuslab.yaml.YamlError

class BaseDecoderErrorSuite extends munit.FunSuite {

  def assertError[T](obtained: Either[YamlError, T], expected: String): Unit =
    obtained match {
      case r @ Right(value)       => fail(s"Get $r, expected Left")
      case Left(error: YamlError) => assertNoDiff(error.msg, expected)
    }

}
