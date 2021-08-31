package org.virtuslab.yaml

import org.virtuslab.yaml.internal.load.reader.Position

/**
  * ADT that corresponds to the YAML representation graph nodes https://yaml.org/spec/1.2/spec.html#id2764044
*/
sealed trait Node:
  def pos: Option[Position]

object Node:
  final case class ScalarNode(value: String, pos: Option[Position] = None) extends Node

  final case class SequenceNode(nodes: Seq[Node], pos: Option[Position] = None) extends Node
  object SequenceNode:
    def apply(nodes: Node*): SequenceNode = SequenceNode(nodes, None)

  final case class MappingNode(mappings: Seq[KeyValueNode], pos: Option[Position] = None)
      extends Node
  object MappingNode:
    def apply(nodes: KeyValueNode*): MappingNode = MappingNode(nodes, None)

  final case class KeyValueNode(key: ScalarNode, value: Node, pos: Option[Position] = None)
      extends Node
