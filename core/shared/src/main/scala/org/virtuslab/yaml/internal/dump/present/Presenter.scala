package org.virtuslab.yaml.internal.dump.present

import org.virtuslab.yaml.internal.load.parse.EventKind

trait Presenter {
  def asString(events: Seq[EventKind]): String
}
