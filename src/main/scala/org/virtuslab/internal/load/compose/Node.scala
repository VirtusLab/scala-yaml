package org.virtuslab.internal.load.compose

sealed trait Node

object Node:
  case class Scalar(value: String) extends Node

  case class SequenceNode(nodes: Seq[Node]) extends Node
  case object SequenceNode:
    final val empty: SequenceNode = SequenceNode(Seq.empty)

  case class MappingdNode(mappings: Seq[Mapping]) extends Node
  case object MappingNode:
    final val empty: MappingdNode = MappingdNode(Seq.empty)

  case class Mapping(key: Scalar, value: Node) extends Node
