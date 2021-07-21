package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.reader.Reader
import org.virtuslab.yaml.internal.load.reader.token.Token
trait Parser:
  def getEvents(in: Reader): Either[YamlError, Seq[Event]]
