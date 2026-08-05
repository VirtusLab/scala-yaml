package org.virtuslab.yaml.internal.load.compose

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.ListMap
import scala.util.control.NoStackTrace

import org.virtuslab.yaml.ComposerError
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Range
import org.virtuslab.yaml.Tag
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.parse.Anchor
import org.virtuslab.yaml.internal.load.parse.Event
import org.virtuslab.yaml.internal.load.parse.EventKind

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
  // A lightweight mutable wrapper avoiding `Result` allocation per event.
  private class Context(var events: List[Event])

  // Internal exception used to fast-fail without paying the stack-trace generation cost
  private case class ComposerException(err: YamlError) extends RuntimeException with NoStackTrace

  override def fromEvents(events: List[Event]): Either[YamlError, Node] = events match {
    case Nil => new Left(ComposerError("No events available"))
    case _ =>
      try new Right(composeNode(new Context(events), mutable.Map.empty))
      catch {
        case ComposerException(err) => new Left(err)
      }
  }

  override def multipleFromEvents(events: List[Event]): Either[YamlError, List[Node]] = {
    val aliases = mutable.Map.empty[Anchor, Node]
    val ctx     = new Context(events)

    @tailrec
    def go(out: mutable.ListBuffer[Node]): List[Node] =
      ctx.events match {
        case e :: tail =>
          e.kind match {
            case _: EventKind.StreamEnd.type =>
              ctx.events = tail
              out.toList
            case _: EventKind.StreamStart.type | _: EventKind.DocumentEnd =>
              ctx.events = tail
              go(out)
            case _ =>
              out.addOne(composeNode(ctx, aliases))
              go(out)
          }
        case Nil => out.toList
      }

    try new Right(go(new mutable.ListBuffer[Node]))
    catch {
      case ComposerException(err) => new Left(err)
    }
  }

  private def composeNode(
      ctx: Context,
      aliases: mutable.Map[Anchor, Node]
  ): Node = ctx.events match {
    case head :: tail =>
      // Advance the pointer so that recursive calls see the remaining sequence
      ctx.events = tail
      head.kind match {
        case s: EventKind.Scalar =>
          val tag: Tag = s.metadata.tag.getOrElse(Tag.resolveTag(s.value, Some(s.style)))
          val node     = new Node.ScalarNode(s.value, tag, head.pos)
          s.metadata.anchor.foreach(anchor => aliases.put(anchor, node))
          node
        case ss: EventKind.SequenceStart =>
          composeSequenceNode(ctx, ss.metadata.anchor, aliases)
        case ms: EventKind.MappingStart =>
          composeMappingNode(ctx, ms.metadata.anchor, aliases)
        case a: EventKind.Alias =>
          aliases.get(a.id) match {
            case Some(node) => node
            case None =>
              throw ComposerException(ComposerError(s"There is no anchor for ${a.id.anchor} alias"))
          }
        case _: EventKind.StreamStart.type | _: EventKind.DocumentStart =>
          composeNode(ctx, aliases)
        case event =>
          throw ComposerException(ComposerError(s"Expected YAML node, but found: $event"))
      }
    case Nil => throw ComposerException(ComposerError("No events available"))
  }

  private def composeSequenceNode(
      ctx: Context,
      anchorOpt: Option[Anchor],
      aliases: mutable.Map[Anchor, Node]
  ): Node.SequenceNode = {

    @tailrec
    def parseChildren(
        children: mutable.ListBuffer[Node],
        firstChildPos: Option[Range] = None
    ): Node.SequenceNode = {
      ctx.events match {
        case e :: tail =>
          e.kind match {
            case _: EventKind.SequenceEnd.type =>
              ctx.events = tail
              val sequence = new Node.SequenceNode(children.toList, Tag.seq, firstChildPos)
              if (anchorOpt.isDefined) aliases.put(anchorOpt.get, sequence)
              sequence
            case _ =>
              val node = composeNode(ctx, aliases)
              children.addOne(node)
              val nextPos =
                if (firstChildPos.isEmpty) node.pos
                else firstChildPos
              parseChildren(children, nextPos)
          }
        case Nil =>
          throw ComposerException(ComposerError("Not found SequenceEnd event for sequence"))
      }
    }

    parseChildren(new mutable.ListBuffer[Node]())
  }

  private def composeMappingNode(
      ctx: Context,
      anchorOpt: Option[Anchor],
      aliases: mutable.Map[Anchor, Node]
  ): Node.MappingNode = {

    @tailrec
    def parseMappings(
        mappingsBuffer: mutable.ArrayBuffer[(Node, Node)],
        firstChildPos: Option[Range] = None
    ): Node.MappingNode = ctx.events match {
      case e :: tail =>
        e.kind match {
          case _: EventKind.MappingEnd.type =>
            ctx.events = tail
            val mapping =
              new Node.MappingNode(ListMap.from(mappingsBuffer), Tag.map, firstChildPos)
            if (anchorOpt.isDefined) aliases.put(anchorOpt.get, mapping)
            mapping
          case _: EventKind.StreamStart.type | _: EventKind.StreamEnd.type |
              _: EventKind.DocumentStart | _: EventKind.DocumentEnd =>
            throw ComposerException(
              ComposerError(s"Invalid event, got: ${e.kind}, expected Node")
            )
          case _ =>
            val keyNode   = composeNode(ctx, aliases)
            val valueNode = composeNode(ctx, aliases)
            mappingsBuffer.addOne((keyNode, valueNode))
            parseMappings(mappingsBuffer, keyNode.pos)
        }
      case Nil => throw ComposerException(ComposerError("Not found MappingEnd event for mapping"))
    }

    parseMappings(new mutable.ArrayBuffer[(Node, Node)])
  }
}
