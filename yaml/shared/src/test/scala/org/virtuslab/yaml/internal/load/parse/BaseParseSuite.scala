package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.parse.Event.*
import org.virtuslab.yaml.internal.load.reader.Scanner

class BaseParseSuite extends munit.FunSuite:

  /**
   * Checks if events are equal and don't care about positions.
   * Get rid off positions from obtained result and then delegate asserting to munit assertEquals
   **/
  def assertEventsEquals(
      obtained: Either[YamlError, List[Event]],
      expectedEvents: List[Event]
  ): Unit =
    val withoutPosition = obtained.map(events =>
      events.map { e =>
        e match
          case StreamStart(_)            => StreamStart
          case StreamEnd(_)              => StreamEnd
          case e: Event.DocumentStart    => e.copy(pos = None)
          case e: Event.DocumentEnd      => e.copy(pos = None)
          case e: Event.SequenceStart    => e.copy(pos = None)
          case e: Event.SequenceEnd      => e.copy(pos = None)
          case e: Event.MappingStart     => e.copy(pos = None)
          case e: Event.MappingEnd       => e.copy(pos = None)
          case e: Event.FlowMappingStart => e.copy(pos = None)
          case e: Event.FlowMappingEnd   => e.copy(pos = None)
          case e: Event.Scalar           => e.copy(pos = None)
      }
    )
    val expected = Right(expectedEvents)
    assertEquals(withoutPosition, expected)

  extension (yaml: String)
    def events: Either[YamlError, List[Event]] = {
      val reader = Scanner(yaml)
      ParserImpl(reader).getEvents()
    }
