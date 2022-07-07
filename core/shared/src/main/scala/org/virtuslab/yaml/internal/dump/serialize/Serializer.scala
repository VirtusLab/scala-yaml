package org.virtuslab.yaml.internal.dump.serialize

import org.virtuslab.yaml.Node
import org.virtuslab.yaml.internal.load.parse.EventKind

trait Serializer {
  def toEvents(node: Node): Seq[EventKind]
}
