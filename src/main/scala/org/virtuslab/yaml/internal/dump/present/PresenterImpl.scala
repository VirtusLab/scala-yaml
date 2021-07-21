package org.virtuslab.yaml.internal.dump.present

import org.virtuslab.yaml.internal.load.parse.Event

import scala.annotation.tailrec
import scala.collection.mutable

object PresenterImpl extends Presenter:
  override def asString(events: Seq[Event]): String = {
    val sb      = new StringBuilder
    val stack   = new mutable.Stack[Event]
    val newline = System.lineSeparator()

    var toplevelNode = true // toplevel node should't insert newline and increase indent
    var indent       = 0

    def parseNode(events: List[Event]): List[Event] =
      events match
        case head :: tail =>
          head match
            case Event.MappingStart =>
              insertSequencePadding()
              pushAndIncreaseIndent(Event.MappingStart)
              parseMapping(tail)
            case Event.SequenceStart =>
              insertSequencePadding()
              pushAndIncreaseIndent(Event.SequenceStart)
              parseSequence(tail)
            case Event.Scalar(value, _) =>
              insertSequencePadding()
              // todo escape string using doublequotes
              sb.append(value)
              sb.append(newline)
              tail
            case Event.DocumentStart(_) => parseNode(tail)
            case Event.DocumentEnd(_)   => parseNode(tail)
            case _                      => events
        case Nil => Nil

    @tailrec
    def parseMapping(events: List[Event]): List[Event] = {
      events match
        case Event.MappingEnd :: tail =>
          popAndDecreaseIndent()
          tail
        case Event.Scalar(value, _) :: tail =>
          appendKey(value)
          val rest = parseNode(tail)
          parseMapping(rest)
        case _ => events
    }

    @tailrec
    def parseSequence(events: List[Event]): List[Event] =
      events match
        case Event.SequenceEnd :: tail =>
          popAndDecreaseIndent()
          tail
        case _ =>
          val rest = parseNode(events)
          parseSequence(rest)

    def appendKey(value: String) =
      sb.append(" " * indent)
      sb.append(value)
      sb.append(": ")

    def insertSequencePadding() = stack.headOption match
      case Some(Event.SequenceStart) =>
        sb.append(" " * indent)
        sb.append("- ")
      case _ => ()

    def pushAndIncreaseIndent(event: Event) =
      if toplevelNode then toplevelNode = false
      else
        indent += 2
        sb.append(System.lineSeparator())
      stack.prepend(event)

    def popAndDecreaseIndent() =
      indent -= 2
      stack.pop()

    parseNode(events.toList)
    sb.result()
  }
