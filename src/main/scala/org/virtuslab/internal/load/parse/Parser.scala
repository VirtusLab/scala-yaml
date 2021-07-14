package org.virtuslab.internal.load.parse

import org.virtuslab.internal.YamlError
import org.virtuslab.internal.load.reader.Reader
import org.virtuslab.internal.load.reader.token.Token
trait Parser:
  def getEvents(in: Reader): Either[YamlError, Seq[Event]]
