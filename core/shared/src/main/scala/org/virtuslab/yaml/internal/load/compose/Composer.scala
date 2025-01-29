package org.virtuslab.yaml.internal.load.compose

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.ListMap

import org.virtuslab.yaml.ComposerError
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Position
import org.virtuslab.yaml.Range
import org.virtuslab.yaml.Tag
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.parse.Anchor
import org.virtuslab.yaml.internal.load.parse.Event
import org.virtuslab.yaml.internal.load.parse.EventKind
import org.virtuslab.yaml.internal.load.parse.NodeEventMetadata
import org.virtuslab.yaml.internal.load.parse.ParserImpl

/**
 * Composing takes a series of serialization events and produces a representation graph.
 * It can fail due to any of several reasons e.g. unexpected event.
 * Returns either [[YamlError]] or [[Node]](s)
 */
trait Composer {
  def fromEvents(events: List[Event]): Either[YamlError, Node]
  def multipleFromEvents(events: List[Event]): Either[YamlError, List[Node]]
}

object ComposerImpl extends Composer {
  private case class Result[+T](node: T, remaining: List[Event])
  private type ComposeResult[+T] = Either[YamlError, Result[T]]
  // WithPos is used in inner tailrec methods because they also return position of first child
  private type ComposeResultWithPos[T] = Either[YamlError, (Result[T], Option[Range])]

  override def fromEvents(events: List[Event]): Either[YamlError, Node] = events match {
    case Nil => Left(ComposerError("No events available"))
    case _   => composeNode(events, mutable.Map.empty).map(_.node)
  }

  override def multipleFromEvents(events: List[Event]): Either[YamlError, List[Node]] = {
    val aliases = mutable.Map.empty[Anchor, Node]

    @tailrec
    def go(out: List[Node], remaining: List[Event]): Either[YamlError, List[Node]] =
      remaining.headOption.map(_.kind) match {
        case None | Some(EventKind.StreamEnd) =>
          Right(out.reverse)
        case Some(_: EventKind.DocumentEnd) =>
          go(out, remaining.tail)
        case _ =>
          composeNode(remaining, aliases) match {
            case Right(Result(node, tail)) => go(node :: out, tail)
            case Left(error)               => Left(error)
          }
      }

    go(List.empty, events)
  }

  private def composeNode(
      events: List[Event],
      aliases: mutable.Map[Anchor, Node]
  ): ComposeResult[Node] = events match {
    case head :: tail =>
      head.kind match {
        case EventKind.StreamStart | _: EventKind.DocumentStart => composeNode(tail, aliases)
        case EventKind.SequenceStart(NodeEventMetadata(anchor, _)) =>
          composeSequenceNode(tail, anchor, aliases)
        case EventKind.MappingStart(NodeEventMetadata(anchor, _)) =>
          composeMappingNode(tail, anchor, aliases)
        case s: EventKind.Scalar =>
          val tag: Tag = s.metadata.tag.getOrElse(Tag.resolveTag(s.value, Some(s.style)))
          val node     = Node.ScalarNode(s.value, tag, head.pos)
          s.metadata.anchor.foreach(anchor => aliases.put(anchor, node))
          Right(Result(node, tail))
        // todo #88
        case EventKind.Alias(alias) =>
          aliases.get(alias) match {
            case Some(node) => Right(Result(node, tail))
            case None       => Left(ComposerError(s"There is no anchor for $alias alias"))
          }
        case event => Left(ComposerError(s"Expected YAML node, but found: $event"))
      }
    case Nil =>
      Left(ComposerError("No events available"))
  }

  private def composeSequenceNode(
      events: List[Event],
      anchorOpt: Option[Anchor],
      aliases: mutable.Map[Anchor, Node]
  ): ComposeResult[Node.SequenceNode] = {
    @tailrec
    def parseChildren(
        events: List[Event],
        children: List[Node],
        firstChildPos: Option[Range] = None
    ): ComposeResultWithPos[List[Node]] = events match {
      case Nil => Left(ComposerError("Not found SequenceEnd event for sequence"))
      case (Event(EventKind.SequenceEnd, _)) :: tail =>
        Right((Result(children, tail), firstChildPos))
      case _ =>
        composeNode(events, aliases) match {
          case Right(Result(node, rest)) => parseChildren(rest, children :+ node, node.pos)
          case Left(err)                 => Left(err)
        }
    }

    parseChildren(events, Nil).map { case (Result(nodes, rest), pos) =>
      val sequence = Node.SequenceNode(nodes, Tag.seq, pos)
      anchorOpt.foreach(anchor => aliases.put(anchor, sequence))
      Result(sequence, rest)
    }
  }

  private def composeMappingNode(
      events: List[Event],
      anchorOpt: Option[Anchor],
      aliases: mutable.Map[Anchor, Node]
  ): ComposeResult[Node.MappingNode] = {
    @tailrec
    def parseMappings(
        events: List[Event],
        mappings: List[(Node, Node)],
        firstChildPos: Option[Range] = None
    ): ComposeResultWithPos[List[(Node, Node)]] = {
      events match {
        case Nil => Left(ComposerError("Not found MappingEnd event for mapping"))
        case Event(EventKind.MappingEnd, _) :: tail =>
          Right((Result(mappings, tail), firstChildPos))
        case (e @ Event(
              EventKind.StreamStart | EventKind.StreamEnd | _: EventKind.DocumentStart |
              _: EventKind.DocumentEnd,
              _
            )) :: tail =>
          Left(ComposerError(s"Invalid event, got: ${e.kind}, expected Node"))
        case _ =>
          val mapping =
            for {
              key <- composeNode(events, aliases)
              v   <- composeNode(key.remaining, aliases)
            } yield Result((key.node, v.node), v.remaining)
          mapping match {
            case Right(Result(node @ (key, value), rest)) =>
              parseMappings(rest, mappings :+ node, key.pos)
            case Left(err) => Left(err)
          }
      }
    }

    parseMappings(events, Nil).map { case (Result(nodes, rest), pos) =>
      val mapping = Node.MappingNode(ListMap.from(nodes), Tag.map, pos)
      anchorOpt.foreach(anchor => aliases.put(anchor, mapping))
      Result(
        mapping,
        rest
      )
    }
  }
}
