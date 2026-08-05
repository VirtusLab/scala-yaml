package org.virtuslab.yaml.decoder

import org.virtuslab.yaml._

class AliasErrorSuite extends BaseDecoderErrorSuite {

  test("alias referencing an undefined anchor reports the anchor name") {
    assertError(
      "foo: *missing".as[Map[String, String]],
      "There is no anchor for missing alias"
    )
  }

  test("cyclic alias reports the anchor name") {
    assertError(
      "a: &x [*x]".as[Map[String, String]],
      "There is no anchor for x alias"
    )
  }
}
