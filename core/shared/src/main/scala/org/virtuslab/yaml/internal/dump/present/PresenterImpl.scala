package org.virtuslab.yaml.internal.dump.present

import scala.annotation.tailrec
import scala.collection.mutable

import org.virtuslab.yaml.Range
import org.virtuslab.yaml.Tag
import org.virtuslab.yaml.internal.load.parse.EventKind
import org.virtuslab.yaml.internal.load.parse.EventKind._
import org.virtuslab.yaml.internal.load.parse.NodeEventMetadata

object PresenterImpl extends Presenter {
  override def asString(events: Seq[EventKind]): String = {
    val sb      = new StringBuilder
    val stack   = new mutable.Stack[EventKind]
    val newline = System.lineSeparator()

    var toplevelNode = true // toplevel node should't insert newline and increase indent
    var indent       = 0

    def parseNode(events: List[EventKind]): List[EventKind] =
      events match {
        case head :: tail =>
          head match {
            case _: MappingStart =>
              insertSequencePadding()
              pushAndIncreaseIndent(MappingStart())
              parseMapping(tail)
            case _: SequenceStart =>
              insertSequencePadding()
              pushAndIncreaseIndent(SequenceStart())
              parseSequence(tail)
            case Scalar(value, _, NodeEventMetadata(_, tag)) =>
              insertSequencePadding()
              // todo escape string using doublequotes
              if (tag.contains(Tag.nullTag)) sb.append("!!null")
              else sb.append(value)
              sb.append(newline)
              tail
            case DocumentStart(_) => parseNode(tail)
            case DocumentEnd(_)   => parseNode(tail)
            case _                => events
          }
        case Nil => Nil
      }

    @tailrec
    def parseMapping(events: List[EventKind]): List[EventKind] = {
      events match {
        case MappingEnd :: tail =>
          popAndDecreaseIndent()
          tail
        case Scalar(value, _, _) :: tail =>
          appendKey(value)
          val rest = parseNode(tail)
          parseMapping(rest)
        case _ => events
      }
    }

    @tailrec
    def parseSequence(events: List[EventKind]): List[EventKind] =
      events match {
        case SequenceEnd :: tail =>
          popAndDecreaseIndent()
          tail
        case _ =>
          val rest = parseNode(events)
          parseSequence(rest)
      }

    def appendKey(value: String) = {
      sb.append(" " * indent)
      sb.append(value)
      sb.append(": ")
    }

    def insertSequencePadding() = stack.headOption match {
      case Some(_: SequenceStart) =>
        sb.append(" " * indent)
        sb.append("- ")
      case _ => ()
    }

    def pushAndIncreaseIndent(event: EventKind) = {
      if (toplevelNode) toplevelNode = false
      else {
        indent += 2
        sb.append(System.lineSeparator())
      }
      stack.prepend(event)
    }

    def popAndDecreaseIndent() = {
      indent -= 2
      stack.pop()
    }

    parseNode(events.toList)
    sb.result()
  }
}
