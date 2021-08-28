package org.virtuslab.yaml.internal.load.compose

import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Node.*
import org.virtuslab.yaml.internal.load.compose.ComposerImpl
import org.virtuslab.yaml.internal.load.parse.Event
import org.virtuslab.yaml.internal.load.parse.Event.*

/** Examples taken from https://yaml.org/spec/1.2/spec.html#id2759963
  */
class ComposerSuite extends munit.FunSuite:

  test("sequence of scalars") {
    val events = List[Event](
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      Scalar("Mark McGwire"),
      Scalar("Sammy Sosa"),
      Scalar("Ken Griffey"),
      SequenceEnd(),
      DocumentEnd(),
      StreamEnd
    )
    val expected = Right(
      SequenceNode(
        None,
        ScalarNode("Mark McGwire"),
        ScalarNode("Sammy Sosa"),
        ScalarNode("Ken Griffey")
      )
    )

    assertEquals(ComposerImpl.fromEvents(events), expected)
  }

  test("mapping of scalars") {
    val events = List[Event](
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("hr"),
      Scalar("65"),
      Scalar("avg"),
      Scalar("0.278"),
      Scalar("rbi"),
      Scalar("147"),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    val expected = Right(
      MappingNode(
        None,
        KeyValueNode(ScalarNode("hr"), ScalarNode("65")),
        KeyValueNode(ScalarNode("avg"), ScalarNode("0.278")),
        KeyValueNode(ScalarNode("rbi"), ScalarNode("147"))
      )
    )

    assertEquals(ComposerImpl.fromEvents(events), expected)
  }

  test("mapping of sequences") {
    val events = List[Event](
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("american"),
      SequenceStart(),
      Scalar("Boston Red Sox"),
      Scalar("Detroit Tigers"),
      Scalar("New York Yankees"),
      SequenceEnd(),
      Scalar("national"),
      SequenceStart(),
      Scalar("New York Mets"),
      Scalar("Chicago Cubs"),
      Scalar("Atlanta Braves"),
      SequenceEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )
    val expected = Right(
      MappingNode(
        List(
          KeyValueNode(
            ScalarNode("american"),
            SequenceNode(
              None,
              ScalarNode("Boston Red Sox"),
              ScalarNode("Detroit Tigers"),
              ScalarNode("New York Yankees")
            )
          ),
          KeyValueNode(
            ScalarNode("national"),
            SequenceNode(
              None,
              ScalarNode("New York Mets"),
              ScalarNode("Chicago Cubs"),
              ScalarNode("Atlanta Braves")
            )
          )
        )
      )
    )

    assertEquals(ComposerImpl.fromEvents(events), expected)
  }

  test("sequence of mappings") {
    val events = List[Event](
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      MappingStart(),
      Scalar("name"),
      Scalar("Mark McGwire"),
      Scalar("hr"),
      Scalar("65"),
      Scalar("avg"),
      Scalar("0.278"),
      MappingEnd(),
      MappingStart(),
      Scalar("name"),
      Scalar("Sammy Sosa"),
      Scalar("hr"),
      Scalar("63"),
      Scalar("avg"),
      Scalar("0.288"),
      MappingEnd(),
      SequenceEnd(),
      DocumentEnd(),
      StreamEnd
    )
    val expected = Right(
      SequenceNode(
        None,
        MappingNode(
          None,
          KeyValueNode(ScalarNode("name"), ScalarNode("Mark McGwire")),
          KeyValueNode(ScalarNode("hr"), ScalarNode("65")),
          KeyValueNode(ScalarNode("avg"), ScalarNode("0.278"))
        ),
        MappingNode(
          None,
          KeyValueNode(ScalarNode("name"), ScalarNode("Sammy Sosa")),
          KeyValueNode(ScalarNode("hr"), ScalarNode("63")),
          KeyValueNode(ScalarNode("avg"), ScalarNode("0.288"))
        )
      )
    )

    assertEquals(ComposerImpl.fromEvents(events), expected)
  }
