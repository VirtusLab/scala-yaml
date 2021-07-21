package org.virtuslab.yaml.internal.dump.present

import org.virtuslab.yaml.internal.load.parse.Event

trait Presenter:
  def asString(events: Seq[Event]): String
