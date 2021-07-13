package org.virtuslab.internal.load.reader

sealed trait ReaderState:
  def indent: Int

case object ReaderState:

  case class Stream(indent: Int)   extends ReaderState
  case class Document(indent: Int) extends ReaderState
  case class Mapping(indent: Int)  extends ReaderState
  case class Sequence(indent: Int) extends ReaderState
