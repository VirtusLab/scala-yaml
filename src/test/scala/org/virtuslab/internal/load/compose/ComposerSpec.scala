package org.virtuslab.internal.load.compose

import org.virtuslab.internal.load.compose.ComposerImpl
import org.virtuslab.internal.load.parse.Event.*
import org.virtuslab.internal.load.compose.Node.*

class ComposerSuite extends munit.FunSuite:

  test("compose-sequence") {
    val events = List(
      StreamStart,
      DocumentStart,
      SequenceStart,
      Scalar("Mark McGwire"),
      Scalar("Sammy Sosa"),
      Scalar("Ken Griffey"),
      SequenceEnd,
      DocumentEnd,
      StreamEnd
    )
    val expected = Right(
      SequenceNode(
        List(ScalarNode("Mark McGwire"), ScalarNode("Sammy Sosa"), ScalarNode("Ken Griffey"))
      )
    )
    assertEquals(ComposerImpl.fromEvents(events), expected)
  }

  test("compose-mapping") {
    val events = List(
      StreamStart,
      DocumentStart,
      MappingStart,
      Scalar("hr"),
      Scalar("65"),
      Scalar("avg"),
      Scalar("0.278"),
      Scalar("rbi"),
      Scalar("147"),
      MappingEnd,
      DocumentEnd,
      StreamEnd
    )

    val expected = Right(
      MappingNode(
        List(
          Mapping(ScalarNode("hr"), ScalarNode("65")),
          Mapping(ScalarNode("avg"), ScalarNode("0.278")),
          Mapping(ScalarNode("rbi"), ScalarNode("147"))
        )
      )
    )

    assertEquals(ComposerImpl.fromEvents(events), expected)
  }
