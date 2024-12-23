package org.virtuslab.yaml

import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Node._
import org.virtuslab.yaml.internal.load.compose.ComposerImpl
import org.virtuslab.yaml.internal.load.parse.Event
import org.virtuslab.yaml.internal.load.parse.EventKind._
import org.virtuslab.yaml.syntax.YamlPrimitive._

/** Examples taken from https://yaml.org/spec/1.2/spec.html#id2759963
  */
class ComposerSuite extends munit.FunSuite {

  test("sequence of scalars") {
    val events = List(
      StreamStart,
      DocumentStart(),
      SequenceStart(),
      Scalar("Mark McGwire"),
      Scalar("Sammy Sosa"),
      Scalar("Ken Griffey"),
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    ).map(Event(_, None))

    val expected = Right(
      SequenceNode(
        ScalarNode("Mark McGwire"),
        ScalarNode("Sammy Sosa"),
        ScalarNode("Ken Griffey")
      )
    )

    assertEquals(ComposerImpl.fromEvents(events), expected)
  }

  test("mapping of scalars") {
    val events = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("hr"),
      Scalar("65"),
      Scalar("avg"),
      Scalar("0.278"),
      Scalar("rbi"),
      Scalar("147"),
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    ).map(Event(_, None))

    val expected = Right(
      MappingNode(
        ScalarNode("hr")  -> ScalarNode("65"),
        ScalarNode("avg") -> ScalarNode("0.278"),
        ScalarNode("rbi") -> ScalarNode("147")
      )
    )

    assertEquals(ComposerImpl.fromEvents(events), expected)
  }

  test("mapping of sequences") {
    val events = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("american"),
      SequenceStart(),
      Scalar("Boston Red Sox"),
      Scalar("Detroit Tigers"),
      Scalar("New York Yankees"),
      SequenceEnd,
      Scalar("national"),
      SequenceStart(),
      Scalar("New York Mets"),
      Scalar("Chicago Cubs"),
      Scalar("Atlanta Braves"),
      SequenceEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    ).map(Event(_, None))

    val expected = Right(
      MappingNode(
        Map[Node, Node](
          ScalarNode("american") ->
            SequenceNode(
              ScalarNode("Boston Red Sox"),
              ScalarNode("Detroit Tigers"),
              ScalarNode("New York Yankees")
            ),
          ScalarNode("national") ->
            SequenceNode(
              ScalarNode("New York Mets"),
              ScalarNode("Chicago Cubs"),
              ScalarNode("Atlanta Braves")
            )
        )
      )
    )

    assertEquals(ComposerImpl.fromEvents(events), expected)
  }

  test("sequence of mappings") {
    val events = List(
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
      MappingEnd,
      MappingStart(),
      Scalar("name"),
      Scalar("Sammy Sosa"),
      Scalar("hr"),
      Scalar("63"),
      Scalar("avg"),
      Scalar("0.288"),
      MappingEnd,
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    ).map(Event(_, None))

    val expected = Right(
      SequenceNode(
        MappingNode(
          ScalarNode("name") -> ScalarNode("Mark McGwire"),
          ScalarNode("hr")   -> ScalarNode("65"),
          ScalarNode("avg")  -> ScalarNode("0.278")
        ),
        MappingNode(
          ScalarNode("name") -> ScalarNode("Sammy Sosa"),
          ScalarNode("hr")   -> ScalarNode("63"),
          ScalarNode("avg")  -> ScalarNode("0.288")
        )
      )
    )

    assertEquals(ComposerImpl.fromEvents(events), expected)
  }

  test("multiple documents") {
    val events = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("k1"),
      Scalar("v1"),
      MappingEnd,
      DocumentEnd(explicit = true),
      DocumentStart(explicit = true),
      MappingStart(),
      Scalar("k2"),
      Scalar("v2"),
      MappingEnd,
      DocumentEnd(explicit = true),
      DocumentStart(explicit = true),
      MappingStart(),
      Scalar("k3"),
      Scalar("v3"),
      MappingEnd,
      DocumentEnd(explicit = true),
      StreamEnd
    ).map(Event(_, None))

    val expected = Right(
      List(
        MappingNode(ScalarNode("k1") -> ScalarNode("v1")),
        MappingNode(ScalarNode("k2") -> ScalarNode("v2")),
        MappingNode(ScalarNode("k3") -> ScalarNode("v3"))
      )
    )

    assertEquals(ComposerImpl.multipleFromEvents(events), expected)
  }
}
