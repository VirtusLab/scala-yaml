package org.virtuslab.yaml

/**
  * ADT that corresponds to the YAML representation graph nodes https://yaml.org/spec/1.2/spec.html#id2764044
*/
sealed trait Node

object Node:
  final case class ScalarNode(value: String) extends Node

  final case class SequenceNode(nodes: Seq[Node]) extends Node
  object SequenceNode:
    final val empty: SequenceNode                     = SequenceNode(Seq.empty)
    def apply(node: Node, nodes: Node*): SequenceNode = SequenceNode(node :: nodes.toList)

  final case class MappingNode(mappings: Seq[KeyValueNode]) extends Node
  object MappingNode:
    final val empty: MappingNode = MappingNode(Seq.empty)
    def apply(node: KeyValueNode, nodes: KeyValueNode*): MappingNode = MappingNode(
      node :: nodes.toList
    )

  final case class KeyValueNode(key: ScalarNode, value: Node) extends Node
