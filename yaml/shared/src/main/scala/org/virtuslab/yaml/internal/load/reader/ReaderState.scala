package org.virtuslab.yaml.internal.load.reader

sealed trait ReaderState:
  def indent: Int

case object ReaderState:
  final case class Document(indent: Int)     extends ReaderState
  final case class Mapping(indent: Int)      extends ReaderState
  final case class Sequence(indent: Int)     extends ReaderState
  final case class FlowMapping(indent: Int)  extends ReaderState
  final case class FlowSequence(indent: Int) extends ReaderState
