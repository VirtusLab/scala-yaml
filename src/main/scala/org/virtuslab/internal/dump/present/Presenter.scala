package org.virtuslab.internal.dump.present

import org.virtuslab.internal.load.parse.Event

trait Presenter:
  def asString(events: Seq[Event]): String
