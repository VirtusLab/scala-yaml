package org.virtuslab.internal.load.parse

import org.virtuslab.internal.YamlError
import org.virtuslab.internal.load.reader.YamlReader
import org.virtuslab.internal.load.reader.token.Token

case class ParserCtx(
    state: Token
)

trait Parser:
  def getEvents(in: YamlReader): Either[YamlError, Seq[Event]]
