package org.virtuslab.yaml.internal.dump.present

import org.virtuslab.yaml._
import org.virtuslab.yaml.internal.load.parse.EventKind._
import org.virtuslab.yaml.internal.load.parse.NodeEventMetadata
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class PresenterBugsSuite extends munit.FunSuite {

  private val nl = System.lineSeparator()

  private def presentScalar(
      value: String,
      tag: Tag = Tag.str,
      style: ScalarStyle = ScalarStyle.Plain
  ): String =
    PresenterImpl.asString(
      Seq(
        DocumentStart(),
        Scalar(value, style, NodeEventMetadata(tag = Some(tag))),
        DocumentEnd()
      )
    )

  // -----------------------------------------------------------------------
  // Bug 1: Trailing whitespace check is inside the `case _` default branch.
  //
  // The trailing-whitespace check in requiresDoubleQuoting only runs when
  // the first character doesn't match any explicit case. Strings like
  // "true " (first char 't') skip it, so trailing whitespace is not detected.
  // YAML strips trailing whitespace on plain scalars (§7.3.3), causing
  // data corruption.
  // -----------------------------------------------------------------------

  test("bug1: trailing whitespace on boolean-like string must be quoted") {
    val result = presentScalar("true ")
    assert(
      result.contains("\"true \""),
      s"Expected 'true ' to be double-quoted, but got: $result"
    )
  }

  test("bug1: trailing whitespace on number-like string must be quoted") {
    val result = presentScalar("123 ")
    assert(
      result.contains("\"123 \""),
      s"Expected '123 ' to be double-quoted, but got: $result"
    )
  }

  test("bug1: trailing whitespace on null-like string must be quoted") {
    val result = presentScalar("null ")
    assert(
      result.contains("\"null \""),
      s"Expected 'null ' to be double-quoted, but got: $result"
    )
  }

  test("bug1: trailing whitespace on float-like string must be quoted") {
    val result = presentScalar("3.14 ")
    assert(
      result.contains("\"3.14 \""),
      s"Expected '3.14 ' to be double-quoted, but got: $result"
    )
  }

  // -----------------------------------------------------------------------
  // Bug 2: Trailing colon not handled.
  //
  // "a:" is not quoted. Per §7.3.3 rule [130], `:` in a plain scalar
  // requires a following ns-plain-safe character. A trailing colon
  // (followed by newline/EOF) fails that lookahead and gets parsed as
  // a mapping value indicator.
  // -----------------------------------------------------------------------

  test("bug2: trailing colon must be quoted") {
    val result = presentScalar("a:")
    assert(
      result.contains("\"a:\""),
      s"Expected 'a:' to be double-quoted, but got: $result"
    )
  }

  test("bug2: trailing colon followed by nothing should be quoted") {
    val result = presentScalar("host:")
    assert(
      result.contains("\"host:\""),
      s"Expected 'host:' to be double-quoted, but got: $result"
    )
  }

  // -----------------------------------------------------------------------
  // Bug 4: `tag.exists(_ eq Tag.str)` uses reference equality.
  //
  // Per §3.2.1.3, tags must be compared by character-by-character string
  // comparison. A Tag constructed for "tag:yaml.org,2002:str" that isn't
  // the Tag.str singleton falls to the `else sb.append(value)` branch:
  // unquoted and unescaped.
  // -----------------------------------------------------------------------

  test("bug4: non-singleton str tag must still trigger quoting logic") {
    val nonSingletonStr = CoreSchemaTag("tag:yaml.org,2002:str")
    assert(
      nonSingletonStr != Tag.str || !(nonSingletonStr eq Tag.str),
      "Precondition: nonSingletonStr should not be reference-equal to Tag.str"
    )
    val result = presentScalar("true", tag = nonSingletonStr)
    assert(
      result.contains("\"true\""),
      s"Expected 'true' with non-singleton str tag to be double-quoted, but got: $result"
    )
  }

  test("bug4: non-singleton null tag must still trigger null rendering") {
    val nonSingletonNull = CoreSchemaTag("tag:yaml.org,2002:null")
    val result           = presentScalar("", tag = nonSingletonNull)
    assert(
      result.contains("!!null"),
      s"Expected empty string with non-singleton null tag to render as !!null, but got: $result"
    )
  }

  test("bug4: scalar with no tag should still apply quoting for ambiguous values") {
    val events = Seq(
      DocumentStart(),
      Scalar("true", ScalarStyle.Plain, NodeEventMetadata.empty),
      DocumentEnd()
    )
    val result = PresenterImpl.asString(events)
    assert(
      !result.trim.equals("true"),
      s"Expected 'true' with no tag to be quoted or handled safely, but got: $result"
    )
  }

  // -----------------------------------------------------------------------
  // Bug 6: escapeFolded doesn't handle folded-style semantics.
  //
  // Per §6.5, §8.1.3, in folded style single newlines between non-empty
  // lines are converted to spaces (rule [72] b-as-space). The current
  // implementation is identical to literal style, so content with single
  // newlines will not round-trip correctly.
  // -----------------------------------------------------------------------

//  test("bug6: folded style must round-trip single newlines correctly") {
//    val input = "abc\ndef"
//    val folded = presentScalar(input, style = ScalarStyle.Folded)
//    val parsed = folded.as[String]
//    parsed match {
//      case Right(result) =>
//        assertEquals(
//          result,
//          input,
//          s"Folded scalar did not round-trip. Emitted:\n$folded"
//        )
//      case Left(err) =>
//        fail(s"Failed to parse folded output: ${err.msg}\nEmitted:\n$folded")
//    }
//  }

  // -----------------------------------------------------------------------
  // Bug 7: Block scalars at top level emit at indent == 0.
  //
  // Per §8.1.1.1 and §6.1, block scalar content indentation level is
  // n + m, and the explicit indicator range is 1-9 (rule [163]).
  // At n=0, content must still be indented at least 1 space.
  // -----------------------------------------------------------------------

//  test("bug7: literal block scalar at top level must indent content") {
//    val input = "abc\ndef"
//    val result = presentScalar(input, style = ScalarStyle.Literal)
//    val parsed = result.as[String]
//    parsed match {
//      case Right(value) =>
//        assertEquals(
//          value,
//          input,
//          s"Literal scalar at top level did not round-trip. Emitted:\n$result"
//        )
//      case Left(err) =>
//        fail(s"Failed to parse literal output at top level: ${err.msg}\nEmitted:\n$result")
//    }
//  }

//  test("bug7: folded block scalar at top level must indent content") {
//    val input = "abc\ndef\n"
//    val result = presentScalar(input, style = ScalarStyle.Folded)
//    val parsed = result.as[String]
//    parsed match {
//      case Right(value) =>
//        assertEquals(
//          value,
//          input,
//          s"Folded scalar at top level did not round-trip. Emitted:\n$result"
//        )
//      case Left(err) =>
//        fail(s"Failed to parse folded output at top level: ${err.msg}\nEmitted:\n$result")
//    }
//  }

  // -----------------------------------------------------------------------
  // Bug 8: sys.error on non-scalar mapping keys is a regression.
  //
  // Per §3.2.1.1, "keys may be arbitrary nodes." Complex keys are legal
  // YAML and the `?` indicator exists for emitting them. Throwing a bare
  // RuntimeException is non-compliant.
  // -----------------------------------------------------------------------

  test("bug8: mapping (non-scalar) mapping key should not throw") {
    val events = Seq(
      DocumentStart(),
      MappingStart(),
      MappingStart(),
      Scalar("nestedKey", metadata = NodeEventMetadata(tag = Some(Tag.str))),
      Scalar("nestedValue", metadata = NodeEventMetadata(tag = Some(Tag.str))),
      MappingEnd,
      Scalar("value", metadata = NodeEventMetadata(tag = Some(Tag.str))),
      MappingEnd,
      DocumentEnd()
    )
    val result = PresenterImpl.asString(events)
    assertEquals(
      result,
      """?
        |  nestedKey: nestedValue
        |: value
        |""".stripMargin
    )
  }

  test("bug8: sequence (non-scalar) mapping key should not throw") {
    val events = Seq(
      DocumentStart(),
      MappingStart(),
      SequenceStart(),
      Scalar("nestedValue1", metadata = NodeEventMetadata(tag = Some(Tag.str))),
      Scalar("nestedValue2", metadata = NodeEventMetadata(tag = Some(Tag.str))),
      SequenceEnd,
      Scalar("value", metadata = NodeEventMetadata(tag = Some(Tag.str))),
      MappingEnd,
      DocumentEnd()
    )
    val result = PresenterImpl.asString(events)
    assertEquals(
      result,
      """?
        |  - nestedValue1
        |  - nestedValue2
        |: value
        |""".stripMargin
    )
  }

  // -----------------------------------------------------------------------
  // Roundtrip: per §3.2.1, §3.3.2, §10.3.2, a !!str node whose content
  // matches a non-string resolution pattern (null, true, 3.9, etc.) must
  // be quoted or explicitly tagged. Otherwise re-parsing resolves it to
  // the wrong type.
  // -----------------------------------------------------------------------

  test("roundtrip: string 'null' must survive encode-decode as string") {
    val yaml   = "null".asYaml
    val parsed = yaml.as[String]
    parsed match {
      case Right(value) =>
        assertEquals(value, "null", s"String 'null' did not round-trip. YAML was: $yaml")
      case Left(err) =>
        fail(s"Failed to parse: ${err.msg}")
    }
  }
}
