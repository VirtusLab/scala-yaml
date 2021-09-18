package org.virtuslab.yaml

import org.virtuslab.yaml.Position

/**
  * ADT that corresponds to the YAML representation graph nodes https://yaml.org/spec/1.2/spec.html#id2764044
*/
sealed trait Node:
  def pos: Option[Position]

object Node:
  final case class ScalarNode private[yaml] (value: String, pos: Option[Position] = None)
      extends Node

  object ScalarNode:
    given (String => ScalarNode)            = ScalarNode(_)
    inline given (ScalarNode => ScalarNode) = identity
    def apply(value: String): ScalarNode    = new ScalarNode(value)

  final case class SequenceNode private[yaml] (nodes: Seq[Node], pos: Option[Position] = None)
      extends Node
  object SequenceNode:
    def apply(nodes: Node*): SequenceNode = new SequenceNode(nodes, None)

  final case class MappingNode private[yaml] (
      mappings: Seq[KeyValueNode],
      pos: Option[Position] = None
  ) extends Node

  object MappingNode:
    def apply[K](nodes: (K, Node)*)(using conversion: (K => ScalarNode)): MappingNode =
      val kvn = nodes.map((k, v) => KeyValueNode(conversion(k), v))
      new MappingNode(kvn, None)

  final case class KeyValueNode private[yaml] (
      key: ScalarNode,
      value: Node,
      pos: Option[Position] = None
  ) extends Node

end Node
