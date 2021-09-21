package org.virtuslab.yaml.internal.load.reader

sealed trait ReaderState:
  def indent: Int

object ReaderState:
  final case class Mapping(indent: Int)      extends ReaderState
  final case class Sequence(indent: Int)     extends ReaderState

  final case class FlowMapping(indent: Int)  extends ReaderState
  final case class FlowSequence(indent: Int) extends ReaderState
