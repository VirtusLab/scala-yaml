package org.virtuslab.yaml.internal.load.compose

import org.virtuslab.yaml.ComposerError
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.parse.Event
import org.virtuslab.yaml.internal.load.parse.ParserImpl
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.Position

import scala.annotation.tailrec

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
  private type ComposeResultWithPos[T] = Either[YamlError, (Result[T], Option[Position])]

  override def fromEvents(events: List[Event]): Either[YamlError, Node] = events match
    case Nil => Left(ComposerError("No events available"))
    case _   => composeNode(events).map(_.node)

  private def composeNode(events: List[Event]): ComposeResult[Node] = events match
    case head :: tail =>
      head match
        case _: Event.StreamStart | _: Event.DocumentStart     => composeNode(tail)
        case _: Event.SequenceStart                            => composeSequenceNode(tail)
        case _: Event.MappingStart | _: Event.FlowMappingStart => composeMappingNode(tail)
        case s: Event.Scalar                                   => composeScalarNode(s, tail)
        case event => Left(ComposerError(s"Unexpected event $event"))
    case Nil =>
      Left(ComposerError("No events available"))

  private def composeSequenceNode(events: List[Event]): ComposeResult[Node.SequenceNode] = {
    @tailrec
    def parseChildren(
        events: List[Event],
        children: List[Node],
        firstChildPos: Option[Position] = None
    ): ComposeResultWithPos[List[Node]] = events match
      case Nil => Left(ComposerError("Not found SequenceEnd event for sequence"))
      case (_: Event.SequenceEnd) :: tail => Right((Result(children, tail), firstChildPos))
      case _ =>
        composeNode(events) match
          case Right(node, rest) => parseChildren(rest, children :+ node, node.pos)
          case Left(err)         => Left(err)

    parseChildren(events, Nil).map { case (Result(nodes, rest), pos) =>
      Result(Node.SequenceNode(nodes, pos), rest)
    }
  }

  private def composeMappingNode(events: List[Event]): ComposeResult[Node.MappingNode] = {
    @tailrec
    def parseMappings(
        events: List[Event],
        mappings: List[Node.KeyValueNode],
        firstChildPos: Option[Position] = None
    ): ComposeResultWithPos[List[Node.KeyValueNode]] = {
      events match
        case Nil => Left(ComposerError("Not found MappingEnd event for mapping"))
        case (_: Event.MappingEnd | _: Event.FlowMappingEnd) :: tail =>
          Right((Result(mappings, tail), firstChildPos))
        case (s: Event.Scalar) :: tail =>
          val mapping =
            for
              key <- composeScalarNode(s, tail).map(_.node)
              v   <- composeNode(tail)
            yield Result(Node.KeyValueNode(key, v.node, key.pos), v.remaining)

          mapping match
            case Right(node, rest) => parseMappings(rest, mappings :+ node, node.pos)
            case Left(err)         => Left(err)

        case head :: tail =>
          Left(ComposerError(s"Invalid event, got: $head, expected Scalar"))
    }

    parseMappings(events, Nil).map { case (Result(nodes, rest), pos) =>
      Result(Node.MappingNode(nodes, pos), rest)
    }
  }

  private def composeScalarNode(
      event: Event.Scalar,
      tail: List[Event]
  ): ComposeResult[Node.ScalarNode] = Right(
    Result(Node.ScalarNode(event.value, event.pos), tail)
  )
