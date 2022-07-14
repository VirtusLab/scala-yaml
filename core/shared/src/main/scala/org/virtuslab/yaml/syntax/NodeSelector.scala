package org.virtuslab.yaml.syntax

import org.virtuslab.yaml.Node

sealed trait NodeSelector {
  def show: String
}

object NodeSelector {

  case class IntSelector(index: Int) extends NodeSelector {
    override def show: String = index.toString
  }
  case class StringSelector(field: String) extends NodeSelector {
    override def show: String = field
  }
}
