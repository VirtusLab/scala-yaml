package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.reader.Tokenizer

trait Parser {
  def getNextEvent(): Either[YamlError, Event]
}
