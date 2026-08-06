package org.virtuslab.yaml.internal.load.compose

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.ListMap
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
  private class Context(var events: List[Event], val aliases: java.util.HashMap[Anchor, Node])

  override def fromEvents(events: List[Event]): Either[YamlError, Node] =
    if (events eq Nil) new Left(new ComposerError("No events available"))
    else {
      try new Right(composeNode(new Context(events, new java.util.HashMap)))
      catch {
        case err: ComposerError => new Left(err)
      }
    }

  override def multipleFromEvents(events: List[Event]): Either[YamlError, List[Node]] = {
    val ctx = new Context(events, new java.util.HashMap)

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
              go(out.addOne(composeNode(ctx)))
          }
        case _ => out.toList
      }

    try new Right(go(new mutable.ListBuffer[Node]))
    catch {
      case err: ComposerError => new Left(err)
    }
  }

  @tailrec
  private def composeNode(
      ctx: Context
  ): Node = ctx.events match {
    case head :: tail =>
      // Advance the pointer so that recursive calls see the remaining sequence
      ctx.events = tail
      head.kind match {
        case s: EventKind.Scalar =>
          val tag = s.metadata.tag match {
            case Some(t) => t
            case _       => Tag.resolveTag(s.value, new Some(s.style))
          }
          val node = new Node.ScalarNode(s.value, tag, head.pos)
          s.metadata.anchor match {
            case Some(a) => ctx.aliases.put(a, node)
            case _       =>
          }
          node
        case ss: EventKind.SequenceStart =>
          composeSequenceNode(ctx, ss.metadata.anchor)
        case ms: EventKind.MappingStart =>
          composeMappingNode(ctx, ms.metadata.anchor)
        case a: EventKind.Alias =>
          val node = ctx.aliases.get(a.id)
          if (node eq null) throw new ComposerError(s"There is no anchor for ${a.id} alias")
          node
        case _: EventKind.StreamStart.type | _: EventKind.DocumentStart =>
          composeNode(ctx)
        case event => throw new ComposerError(s"Expected YAML node, but found: $event")
      }
    case _ => throw new ComposerError("No events available")
  }

  private def composeSequenceNode(
      ctx: Context,
      anchorOpt: Option[Anchor]
  ): Node.SequenceNode = {

    @tailrec
    def go(
        children: mutable.ListBuffer[Node],
        firstChildPos: Option[Range]
    ): Node.SequenceNode = ctx.events match {
      case e :: tail =>
        e.kind match {
          case _: EventKind.SequenceEnd.type =>
            ctx.events = tail
            val sequence = new Node.SequenceNode(children.toList, Tag.seq, firstChildPos)
            if (anchorOpt.isDefined) ctx.aliases.put(anchorOpt.get, sequence)
            sequence
          case _ =>
            val node = composeNode(ctx)
            val nextPos =
              if (firstChildPos eq None) node.pos
              else firstChildPos
            go(children.addOne(node), nextPos)
        }
      case _ =>
        throw new ComposerError("Not found SequenceEnd event for sequence")
    }

    go(new mutable.ListBuffer[Node], None)
  }

  private def composeMappingNode(
      ctx: Context,
      anchorOpt: Option[Anchor]
  ): Node.MappingNode = {

    @tailrec
    def go(
        mappings: mutable.Builder[(Node, Node), ListMap[Node, Node]],
        firstChildPos: Option[Range]
    ): Node.MappingNode = ctx.events match {
      case e :: tail =>
        e.kind match {
          case _: EventKind.MappingEnd.type =>
            ctx.events = tail
            val mapping = new Node.MappingNode(mappings.result(), Tag.map, firstChildPos)
            if (anchorOpt.isDefined) ctx.aliases.put(anchorOpt.get, mapping)
            mapping
          case _: EventKind.StreamStart.type | _: EventKind.StreamEnd.type |
              _: EventKind.DocumentStart | _: EventKind.DocumentEnd =>
            throw new ComposerError(s"Invalid event, got: ${e.kind}, expected Node")
          case _ =>
            val keyNode   = composeNode(ctx)
            val valueNode = composeNode(ctx)
            go(mappings.addOne((keyNode, valueNode)), keyNode.pos)
        }
      case _ => throw new ComposerError("Not found MappingEnd event for mapping")
    }

    go(ListMap.newBuilder[Node, Node], None)
  }
}
