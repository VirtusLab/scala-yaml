package org.virtuslab.internal.load.compose

sealed trait Node

object Node:
  case class ScalarNode(value: String) extends Node

  case class SequenceNode(nodes: Seq[Node]) extends Node
  object SequenceNode:
    final val empty: SequenceNode = SequenceNode(Seq.empty)
    def apply(node: Node, nodes: Node*): SequenceNode = SequenceNode(node :: nodes.toList)

  case class MappingNode(mappings: Seq[Mapping]) extends Node
  object MappingNode:
    final val empty: MappingNode = MappingNode(Seq.empty)
    def apply(node: Mapping, nodes: Mapping*): MappingNode = MappingNode(node :: nodes.toList)

  case class Mapping(key: ScalarNode, value: Node) extends Node
