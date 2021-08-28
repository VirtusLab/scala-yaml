package org.virtuslab.yaml

import org.virtuslab.yaml.internal.load.reader.Position

/**
  * ADT that corresponds to the YAML representation graph nodes https://yaml.org/spec/1.2/spec.html#id2764044
*/
sealed trait Node:
  def start: Option[Position]

object Node:
  final case class ScalarNode(value: String, start: Option[Position] = None) extends Node

  final case class SequenceNode(nodes: Seq[Node], start: Option[Position] = None) extends Node
  object SequenceNode:
    def apply(start: Option[Position], nodes: Node*): SequenceNode =
      SequenceNode(nodes, start)

  final case class MappingNode(mappings: Seq[KeyValueNode], start: Option[Position] = None)
      extends Node
  object MappingNode:
    def apply(start: Option[Position], nodes: KeyValueNode*): MappingNode = MappingNode(
      nodes,
      start
    )

  final case class KeyValueNode(key: ScalarNode, value: Node, start: Option[Position] = None)
      extends Node
