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
          case e @ Scalar(_)               => (composeScalarNode(e), tail)
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
      events match {
        case Nil                 => (Left(YamlError("No events available")), events)
        case SequenceEnd :: tail => (Right(children), tail)
        case _ =>
          val (node, rest) = composeNode(events)
          node match {
            case Right(value) => parseChildren(rest, children :+ value)
            case Left(err)    => (Left(err), rest)
          }
      }
    }

    val (result, rest) = parseChildren(events, Nil)
    val ret = for {
      children <- result
    } yield SequenceNode(children)
    (ret, rest)
  }

  private def composeMappingNode(events: List[Event]): ComposeResult = {
    ???
  }

  private def composeScalarNode(event: Scalar): Either[YamlError, Node] = Right(
    ScalarNode(event.value)
  )
