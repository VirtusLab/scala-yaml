package org.virtuslab.yaml.internal.load.reader

sealed trait ReaderState
case object ReaderState:
  case object Stream   extends ReaderState
  case object Document extends ReaderState
  sealed trait ReaderStateWithIndent extends ReaderState:
    def indent: Int
  final case class Mapping(indent: Int)      extends ReaderStateWithIndent
  final case class Sequence(indent: Int)     extends ReaderStateWithIndent
  final case class FlowMapping(indent: Int)  extends ReaderStateWithIndent
  final case class FlowSequence(indent: Int) extends ReaderStateWithIndent
