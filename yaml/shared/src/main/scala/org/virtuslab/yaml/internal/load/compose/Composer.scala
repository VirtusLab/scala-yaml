package org.virtuslab.yaml.internal.load.compose

import scala.annotation.tailrec

import org.virtuslab.yaml.ComposerError
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Position
import org.virtuslab.yaml.Range
import org.virtuslab.yaml.Tag
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.parse.Event
import org.virtuslab.yaml.internal.load.parse.EventKind
import org.virtuslab.yaml.internal.load.parse.ParserImpl
import org.virtuslab.yaml.internal.load.reader.Scanner

/**
 * Composing takes a series of serialization events and produces a representation graph. 
 * It can fail due to any of several reasons e.g. unexpected event.
 * Returns either [[YamlError]] or [[Node]]
 */
trait Composer:
  def fromEvents(events: List[Event]): Either[YamlError, Node]

object ComposerImpl extends Composer:
  private case class Result[+T](node: T, remaining: List[Event])
  private type ComposeResult[+T] = Either[YamlError, Result[T]]
  // WithPos is used in inner tailrec methods because they also return position of first child
  private type ComposeResultWithPos[T] = Either[YamlError, (Result[T], Option[Range])]

  override def fromEvents(events: List[Event]): Either[YamlError, Node] = events match
    case Nil => Left(ComposerError("No events available"))
    case _   => composeNode(events).map(_.node)

  private def composeNode(events: List[Event]): ComposeResult[Node] = events match
    case head :: tail =>
      head.kind match
        case EventKind.StreamStart | _: EventKind.DocumentStart        => composeNode(tail)
        case _: EventKind.SequenceStart                                => composeSequenceNode(tail)
        case _: EventKind.MappingStart | _: EventKind.FlowMappingStart => composeMappingNode(tail)
        case s: EventKind.Scalar =>
          val tag: Tag = s.metadata.tag.getOrElse(Tag.resolveTag(s.value))
          Right(Result(Node.ScalarNode(s.value, tag, head.pos), tail))
        // todo #88
        case _: EventKind.Alias => Left(ComposerError(s"Aliases aren't currently supported"))
        case event              => Left(ComposerError(s"Expected YAML node, but found: $event"))
    case Nil =>
      Left(ComposerError("No events available"))

  private def composeSequenceNode(events: List[Event]): ComposeResult[Node.SequenceNode] = {
    @tailrec
    def parseChildren(
        events: List[Event],
        children: List[Node],
        firstChildPos: Option[Range] = None
    ): ComposeResultWithPos[List[Node]] = events match
      case Nil => Left(ComposerError("Not found SequenceEnd event for sequence"))
      case (Event(EventKind.SequenceEnd, _)) :: tail =>
        Right((Result(children, tail), firstChildPos))
      case _ =>
        composeNode(events) match
          case Right(node, rest) => parseChildren(rest, children :+ node, node.pos)
          case Left(err)         => Left(err)

    parseChildren(events, Nil).map { case (Result(nodes, rest), pos) =>
      Result(Node.SequenceNode(nodes, Tag.seq, pos), rest)
    }
  }

  private def composeMappingNode(events: List[Event]): ComposeResult[Node.MappingNode] = {
    @tailrec
    def parseMappings(
        events: List[Event],
        mappings: List[(Node, Node)],
        firstChildPos: Option[Range] = None
    ): ComposeResultWithPos[List[(Node, Node)]] = {
      events match
        case Nil => Left(ComposerError("Not found MappingEnd event for mapping"))
        case Event(EventKind.MappingEnd | EventKind.FlowMappingEnd, _) :: tail =>
          Right((Result(mappings, tail), firstChildPos))
        case (e @ Event(
              EventKind.StreamStart | EventKind.StreamEnd | _: EventKind.DocumentStart |
              _: EventKind.DocumentEnd,
              _
            )) :: tail =>
          Left(ComposerError(s"Invalid event, got: ${e.kind}, expected Node"))
        case _ =>
          val mapping =
            for
              key <- composeNode(events)
              v   <- composeNode(key.remaining)
            yield Result((key.node, v.node), v.remaining)
          mapping match
            case Right(node @ (key, value), rest) => parseMappings(rest, mappings :+ node, key.pos)
            case Left(err)                        => Left(err)
    }

    parseMappings(events, Nil).map { case (Result(nodes, rest), pos) =>
      Result(
        Node.MappingNode(nodes.toMap, Tag.map, pos),
        rest
      )
    }
  }
