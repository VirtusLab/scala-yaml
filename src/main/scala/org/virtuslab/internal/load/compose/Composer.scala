package org.virtuslab.internal.load.compose

import org.virtuslab.internal.YamlError
import org.virtuslab.internal.load.parse.{Event, ParserImpl}
import org.virtuslab.internal.load.compose.Node
import org.virtuslab.internal.load.reader.YamlReader

import scala.annotation.tailrec

trait Composer:
  def compose(yaml: String): Either[YamlError, Node]
  def compose(reader: YamlReader): Either[YamlError, Node]

object ComposerImpl extends Composer with NodeTransform:

  type ComposeResult[T] = Either[YamlError, (T, List[Event])]

  override def compose(yaml: String): Either[YamlError, Node] = compose(YamlReader(yaml))

  override def compose(reader: YamlReader): Either[YamlError, Node] =
    for
      events <- ParserImpl.getEvents(reader)
      node   <- fromEvents(events)
    yield node

  override def fromEvents(events: List[Event]): Either[YamlError, Node] = events match
    case Nil => Left(YamlError("No events available"))
    case _   => composeNode(events).map((node, _) => node)

  private def composeNode(events: List[Event]): ComposeResult[Node] = events match
    case head :: tail =>
      head match
        case Event.StreamStart | Event.DocumentStart => composeNode(tail)
        case Event.SequenceStart                     => composeSequenceNode(tail)
        case Event.MappingStart                      => composeMappingNode(tail)
        case s: Event.Scalar                         => composeScalarNode(s, tail)
        case event                                   => Left(YamlError(s"Unexpected event $event"))
    case Nil =>
      Left(YamlError("No events available"))

  private def composeSequenceNode(events: List[Event]): ComposeResult[Node.SequenceNode] = {
    @tailrec
    def parseChildren(
        events: List[Event],
        children: List[Node]
    ): ComposeResult[List[Node]] = events match
      case Nil                       => Left(YamlError("Not found SequenceEnd event for sequence"))
      case Event.SequenceEnd :: tail => Right((children, tail))
      case _ =>
        composeNode(events) match
          case Right(node, rest) => parseChildren(rest, children :+ node)
          case Left(err)         => Left(err)

    parseChildren(events, Nil).map((nodes, rest) => (Node.SequenceNode(nodes), rest))
  }

  private def composeMappingNode(events: List[Event]): ComposeResult[Node.MappingNode] = {
    @tailrec
    def parseMappings(
        events: List[Event],
        mappings: List[Node.KeyValueNode]
    ): ComposeResult[List[Node.KeyValueNode]] = {
      events match
        case Nil                      => Left(YamlError("Not found MappingEnd event for mapping"))
        case Event.MappingEnd :: tail => Right((mappings, tail))
        case (s: Event.Scalar) :: tail =>
          val mapping =
            for
              key         <- composeScalarNode(s, tail).map((key, _) => key)
              valueResult <- composeNode(tail)
              (value, rest) = valueResult
            yield (Node.KeyValueNode(key, value), rest)

          mapping match
            case Right(value, rest) => parseMappings(rest, mappings :+ value)
            case Left(err)          => Left(err)

        case head :: tail =>
          Left(YamlError(s"Invalid event, got: $head, expected Scalar"))
    }

    parseMappings(events, Nil).map((nodes, rest) => (Node.MappingNode(nodes), rest))
  }

  private def composeScalarNode(
      event: Event.Scalar,
      tail: List[Event]
  ): ComposeResult[Node.ScalarNode] = Right(
    (Node.ScalarNode(event.value), tail)
  )
