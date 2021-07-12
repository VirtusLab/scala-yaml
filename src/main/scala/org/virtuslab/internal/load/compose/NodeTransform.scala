package org.virtuslab.internal.load.compose

import org.virtuslab.internal.load.parse.Event
import org.virtuslab.internal.YamlError

trait NodeTransform:
  def fromEvents(events: List[Event]): Either[YamlError, Node]
