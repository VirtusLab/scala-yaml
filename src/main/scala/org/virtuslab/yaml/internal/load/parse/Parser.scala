package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.reader.Reader
trait Parser:
  def getEvents(in: Reader): Either[YamlError, Seq[Event]]
