package org.virtuslab.yaml

import org.virtuslab.yaml.Range
import org.virtuslab.yaml.Tag
import org.virtuslab.yaml.syntax.YamlPrimitive

/**
  * ADT that corresponds to the YAML representation graph nodes https://yaml.org/spec/1.2/spec.html#id2764044
*/
sealed trait Node:
  private[yaml] def pos: Option[Range]
  def tag: Tag

object Node:
  final case class ScalarNode private[yaml] (value: String, tag: Tag, pos: Option[Range] = None)
      extends Node

  object ScalarNode:
    def apply(value: String): ScalarNode = new ScalarNode(value, Tag.resolveTag(value))
    def unapply(node: ScalarNode): Option[(String, Tag)] = Some((node.value, node.tag))
  end ScalarNode

  final case class SequenceNode private[yaml] (
      nodes: Seq[Node],
      tag: Tag,
      pos: Option[Range] = None
  ) extends Node
  object SequenceNode:
    def apply(nodes: Node*): SequenceNode = new SequenceNode(nodes, Tag.seq, None)
    def apply(first: YamlPrimitive, rest: YamlPrimitive*): SequenceNode =
      val nodes: List[YamlPrimitive] = (first :: rest.toList)
      new SequenceNode(nodes.map(_.node), Tag.seq, None)
    def unapply(node: SequenceNode): Option[(Seq[Node], Tag)] = Some((node.nodes, node.tag))
  end SequenceNode

  final case class MappingNode private[yaml] (
      mappings: Map[Node, Node],
      tag: Tag,
      pos: Option[Range] = None
  ) extends Node
  object MappingNode:
    def apply(mappings: Map[Node, Node]): MappingNode = MappingNode(mappings, Tag.map, None)
    def apply(mappings: (Node, Node)*): MappingNode   = MappingNode(mappings.toMap, Tag.map, None)
    def apply(
        first: (YamlPrimitive, YamlPrimitive),
        rest: (YamlPrimitive, YamlPrimitive)*
    ): MappingNode =
      val primitives = (first :: rest.toList)
      val mappings   = primitives.map((k, v) => (k.node -> v.node)).toMap
      new MappingNode(mappings, Tag.map, None)
    def unapply(node: MappingNode): Option[(Map[Node, Node], Tag)] = Some((node.mappings, node.tag))
  end MappingNode
end Node

private object TagResolver {
  val nullPattern   = "null|Null|NULL|~".r
  val boolean       = "true|True|TRUE|false|False|FALSE".r
  val int10         = "[-+]?[0-9]+".r
  val int8          = "0o[0-7]+".r
  val int16         = "0x[0-9a-fA-F]+".r
  val float         = "[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?".r
  val minusInfinity = "-(\\.inf|\\.Inf|\\.INF)".r
  val plusInfinity  = "\\+?(\\.inf|\\.Inf|\\.INF)".r

  def resolveTag(value: String) =
    value match
      case null              => 1
      case nullPattern(_*)   => 2
      case boolean(_*)       => 3
      case int10(_*)         => 4
      case int8(_*)          => 5
      case int16(_*)         => 6
      case float(_*)         => 7
      case minusInfinity(_*) => 8
      case plusInfinity(_*)  => 9
}
