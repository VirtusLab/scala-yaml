package org.virtuslab.internal.dump.serialize

import org.virtuslab.internal.load.compose.Node
import org.virtuslab.internal.load.parse.Event

trait Serializer:
  def toEvents(node: Node): Seq[Event]
