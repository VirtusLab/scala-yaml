package org.virtuslab.yaml

import org.virtuslab.yaml.Range
import org.virtuslab.yaml.Tag
import org.virtuslab.yaml.syntax.NodeSelector
import org.virtuslab.yaml.syntax.YamlPrimitive

/**
  * ADT that corresponds to the YAML representation graph nodes https://yaml.org/spec/1.2/spec.html#id2764044
*/
sealed trait Node {
  def pos: Option[Range]
  def tag: Tag
  def as[T](implicit
      c: YamlDecoder[T],
      settings: LoadSettings = LoadSettings.empty
  ): Either[YamlError, T] =
    c.construct(this)

  def modify(index: Int): NodeVisitor = NodeVisitor(this, List(NodeSelector.IntSelector(index)))
  def modify(field: String): NodeVisitor =
    NodeVisitor(this, List(NodeSelector.StringSelector(field)))
}

object Node {

  final case class ScalarNode private[yaml] (value: String, tag: Tag, pos: Option[Range] = None)
      extends Node

  object ScalarNode {
    def apply(value: String): ScalarNode = new ScalarNode(value, Tag.resolveTag(value))
    def unapply(node: ScalarNode): Option[(String, Tag)] = Some((node.value, node.tag))
  }

  final case class SequenceNode private[yaml] (
      nodes: Seq[Node],
      tag: Tag,
      pos: Option[Range] = None
  ) extends Node
  object SequenceNode {
    def apply(nodes: Node*): SequenceNode = new SequenceNode(nodes, Tag.seq, None)
    def apply(first: YamlPrimitive, rest: YamlPrimitive*): SequenceNode = {
      val nodes: List[YamlPrimitive] = (first :: rest.toList)
      new SequenceNode(nodes.map(_.node), Tag.seq, None)
    }
    def unapply(node: SequenceNode): Option[(Seq[Node], Tag)] = Some((node.nodes, node.tag))
  }

  final case class MappingNode private[yaml] (
      mappings: Map[Node, Node],
      tag: Tag,
      pos: Option[Range] = None
  ) extends Node
  object MappingNode {
    def apply(mappings: Map[Node, Node]): MappingNode = MappingNode(mappings, Tag.map, None)
    def apply(mappings: (Node, Node)*): MappingNode   = MappingNode(mappings.toMap, Tag.map, None)
    def apply(
        first: (YamlPrimitive, YamlPrimitive),
        rest: (YamlPrimitive, YamlPrimitive)*
    ): MappingNode = {
      val primitives = (first :: rest.toList)
      val mappings   = primitives.map { case (k, v) => (k.node -> v.node) }.toMap
      new MappingNode(mappings, Tag.map, None)
    }
    def unapply(node: MappingNode): Option[(Map[Node, Node], Tag)] = Some((node.mappings, node.tag))
  }

  implicit class ModifyOps(val either: Either[ModifyError, Node]) extends AnyVal {
    def modify(index: Int): Either[ModifyError, NodeVisitor]    = either.map(_.modify(index))
    def modify(field: String): Either[ModifyError, NodeVisitor] = either.map(_.modify(field))
  }

}
