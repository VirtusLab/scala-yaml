package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.reader.Tokenizer
trait Parser:
  def getEvents(in: Tokenizer): Either[YamlError, Seq[Event]]
