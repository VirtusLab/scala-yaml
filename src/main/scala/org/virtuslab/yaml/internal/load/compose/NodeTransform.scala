package org.virtuslab.yaml.internal.load.compose

import org.virtuslab.yaml.{Node, YamlError}
import org.virtuslab.yaml.internal.load.parse.Event

trait NodeTransform:
  def fromEvents(events: List[Event]): Either[YamlError, Node]
