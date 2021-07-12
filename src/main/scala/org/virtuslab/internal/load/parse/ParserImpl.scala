package org.virtuslab.internal.load.parse

import scala.annotation.tailrec
import org.virtuslab.internal.load.reader.YamlReader
import org.virtuslab.internal.YamlError
import org.virtuslab.internal.load.reader.token.Token

object ParserImpl extends Parser:
  def getEvents(
      in: YamlReader
  ): Either[YamlError, List[Event]] = {
    @tailrec
    def loop(acc: List[Event]): List[Event] = {
      val (event) = ParserImpl.getNextEvent(in)
      if event != Event.StreamEnd then loop(acc :+ event)
      else acc :+ event
    }
    Right(loop(List(Event.StreamStart, Event.DocumentStart)))
  }

  def getNextEvent(in: YamlReader): Event = {
    in.getToken() match {
      case Token.StreamStart   => Event.StreamStart
      case Token.DocumentStart => Event.DocumentStart
      case Token.MappingStart  => Event.MappingStart
      case Token.MappingEnd    => Event.MappingEnd
      case Token.SequenceStart => Event.SequenceStart
      case Token.SequenceEnd   => Event.SequenceEnd
      case Token.Scalar(v, _)  => Event.Scalar(v)
      case Token.DocumentEnd   => Event.DocumentEnd
      case Token.StreamEnd     => Event.StreamEnd
      case _                   => ???
    }
  }
