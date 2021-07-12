package org.virtuslab.internal.load.parse

import scala.annotation.tailrec
import org.virtuslab.internal.load.YamlReader
import org.virtuslab.internal.YamlError

object ParserImpl extends Parser:
  def getEvents(
      in: YamlReader,
      ctx: ParserCtx
  ): Either[YamlError, List[Event]] = ???

  def getNextEvent(in: YamlReader, ctx: ParserCtx): (Event, ParserCtx) = ???
