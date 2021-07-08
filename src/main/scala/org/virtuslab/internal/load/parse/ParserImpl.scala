package org.virtuslab.internal.load.parse

import scala.annotation.tailrec
import org.virtuslab.internal.load.YamlReader

object ParserImpl extends Parser:
  def getEvents(
      in: YamlReader,
      ctx: ParserCtx
  ): Either[ParserError, List[Event]] = ???

  def getNextEvent(in: YamlReader, ctx: ParserCtx): (Event, ParserCtx) = ???
