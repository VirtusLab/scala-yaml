package org.virtuslab.yaml.decoder

import org.virtuslab.yaml._

class AnyConstructErrorSuite extends BaseDecoderErrorSuite {

  test("construct error renders the tag value instead of the Tag wrapper") {
    val yaml = "!foo bar"

    yaml.as[Any] match {
      case Left(error: YamlError) =>
        assert(
          error.msg.contains("Could't construct runtime instance of !foo"),
          s"unexpected message: ${error.msg}"
        )
        assert(!error.msg.contains("CustomTag("), s"tag wrapper leaked: ${error.msg}")
      case r @ Right(_) => fail(s"Get $r, expected Left")
    }
  }
}
