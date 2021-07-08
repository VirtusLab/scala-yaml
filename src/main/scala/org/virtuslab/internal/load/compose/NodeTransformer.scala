package org.virtuslab.internal.load.compose

import org.virtuslab.internal.load.parse.Event

import scala.annotation.tailrec

object NodeTransformer extends NodeTransform:
  override def fromEvents(events: Seq[Event]): Either[Any, Node] = ???