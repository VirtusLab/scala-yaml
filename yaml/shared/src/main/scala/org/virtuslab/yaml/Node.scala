package org.virtuslab.yaml

import org.virtuslab.yaml.Position
import org.virtuslab.yaml.syntax.YamlPrimitive

/**
  * ADT that corresponds to the YAML representation graph nodes https://yaml.org/spec/1.2/spec.html#id2764044
*/
sealed trait Node:
  def pos: Option[Position]

object Node:
  final case class ScalarNode(value: String, pos: Option[Position] = None) extends Node

  object ScalarNode:
    def apply(value: String): ScalarNode = new ScalarNode(value)

  final case class SequenceNode(nodes: Seq[Node], pos: Option[Position] = None) extends Node
  object SequenceNode:
    def apply(nodes: Node*): SequenceNode = new SequenceNode(nodes, None)
    def apply(first: YamlPrimitive, rest: YamlPrimitive*): SequenceNode =
      val nodes: List[YamlPrimitive] = (first :: rest.toList)
      new SequenceNode(nodes.map(_.node), None)

  final case class MappingNode(
      mappings: Seq[KeyValueNode],
      pos: Option[Position] = None
  ) extends Node

  object MappingNode:
    def apply(nodes: KeyValueNode*): MappingNode = MappingNode(nodes, None)
    def apply(
        first: (YamlPrimitive, YamlPrimitive),
        rest: (YamlPrimitive, YamlPrimitive)*
    ): MappingNode =
      val nodes = (first :: rest.toList)
      val kvn   = nodes.map((k, v) => KeyValueNode(k.node, v.node))
      new MappingNode(kvn, None)

  final case class KeyValueNode(
      key: Node,
      value: Node,
      pos: Option[Position] = None
  ) extends Node

end Node
