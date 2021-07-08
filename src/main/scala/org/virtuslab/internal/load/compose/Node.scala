package org.virtuslab.internal.load.compose

sealed trait Node

object Node:
  case class ScalarNode(value: String) extends Node

  case class SequenceNode(nodes: Seq[Node]) extends Node
  case object SequenceNode:
    final val empty: SequenceNode = SequenceNode(Seq.empty)

  case class MappingNode(mappings: Seq[Mapping]) extends Node
  case object MappingNode:
    final val empty: MappingNode = MappingNode(Seq.empty)

  case class Mapping(key: ScalarNode, value: Node) extends Node
