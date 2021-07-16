package org.virtuslab.internal.load.reader

sealed trait ReaderState:
  def indent: Int
case object ReaderState:
  case object Stream extends ReaderState:
    override val indent = 0
  case object Document extends ReaderState:
    override val indent = 0
  case class Mapping(indent: Int)  extends ReaderState
  case class Sequence(indent: Int) extends ReaderState
