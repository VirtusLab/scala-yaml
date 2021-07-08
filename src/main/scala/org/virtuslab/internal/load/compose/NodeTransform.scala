package org.virtuslab.internal.load.compose

import org.virtuslab.internal.load.parse.Event

trait NodeTransform:
  def fromEvents(events: Seq[Event]): Either[Any, Node]
