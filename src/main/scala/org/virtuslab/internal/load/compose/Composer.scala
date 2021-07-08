package org.virtuslab.internal.load.compose

import org.virtuslab.internal.load.{YamlReader, StringYamlReader}
import org.virtuslab.internal.load.parse.Event
import org.virtuslab.internal.YamlError
import org.virtuslab.internal.load.parse.Event.{Node => _, *}
import org.virtuslab.internal.load.parse.ParserImpl
import org.virtuslab.internal.load.compose.Node.*
import scala.annotation.tailrec

trait Composer:
  def compose(yaml: String): Either[YamlError, Node]
  def compose(reader: YamlReader): Either[YamlError, Node]

object ComposerImpl extends Composer with NodeTransform:

  type ComposeResult = (Either[YamlError, Node], List[Event])

  override def compose(yaml: String): Either[YamlError, Node] = compose(StringYamlReader(yaml))

  override def compose(reader: YamlReader): Either[YamlError, Node] =
    for
      events <- ParserImpl.getEvents(reader, ???)
      node   <- fromEvents(events)
    yield node

  override def fromEvents(events: List[Event]): Either[YamlError, Node] =
    events match {
      case Nil =>
        Left(YamlError("No events available"))
      case _ =>
        val (node, _) = composeNode(events)
        node
    }

  private def composeNode(events: List[Event]): ComposeResult = {
    events match {
      case head :: tail =>
        head match {
          case StreamStart | DocumentStart => composeNode(tail)
          case SequenceStart               => composeSequenceNode(tail)
          case MappingStart                => composeMappingNode(tail)
          case s: Scalar                   => (composeScalarNode(s), tail)
          case _                           => (Left(YamlError("No events available")), events)
        }
      case Nil =>
        (Left(YamlError("No events available")), events)
    }
  }

  private def composeSequenceNode(events: List[Event]): ComposeResult = {
    @tailrec
    def parseChildren(
        events: List[Event],
        children: List[Node]
    ): (Either[YamlError, List[Node]], List[Event]) = {
      events match
        case Nil                 => (Left(YamlError("No events available")), Nil)
        case SequenceEnd :: tail => (Right(children), tail)
        case _ =>
          val (node, rest) = composeNode(events)
          node match
            case Right(value) => parseChildren(rest, children :+ value)
            case Left(err)    => (Left(err), rest)
    }

    val (result, rest) = parseChildren(events, Nil)
    val node           = result.map(SequenceNode(_))
    (node, rest)
  }

  private def composeMappingNode(events: List[Event]): ComposeResult = {
    @tailrec
    def parseMappings(
        events: List[Event],
        mappings: List[Node.Mapping]
    ): (Either[YamlError, List[Node.Mapping]], List[Event]) = {
      events match
        case Nil                => (Left(YamlError("No events available")), Nil)
        case MappingEnd :: tail => (Right(mappings), tail)
        case (s: Scalar) :: tail =>
          lazy val (eitherValue, rest) = composeNode(tail)
          val mapping =
            for
              key   <- composeScalarNode(s)
              value <- eitherValue
            yield Node.Mapping(key, value)

          mapping match
            case Right(value) => parseMappings(rest, mappings :+ value)
            case Left(err)    => (Left(err), rest)

        case head :: tail =>
          (Left(YamlError(s"Invalid event, got: $head, expected Scalar")), events)
    }

    val (result, rest) = parseMappings(events, Nil)
    val node           = result.map(MappingNode(_))
    (node, rest)
  }

  private def composeScalarNode(event: Scalar): Either[YamlError, ScalarNode] = Right(
    ScalarNode(event.value)
  )
