package org.virtuslab.yaml.internal.dump.serialize

import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Position
import org.virtuslab.yaml.internal.load.parse.EventKind
import org.virtuslab.yaml.internal.load.parse.EventKind.*

object SerializerImpl extends Serializer:
  override def toEvents(node: Node): Seq[EventKind] =
    Seq(DocumentStart()) ++ convertNode(node) ++ Seq(DocumentEnd())

  private def convertNode(node: Node) = node match
    case scalar: Node.ScalarNode     => convertScalarNode(scalar)
    case mapping: Node.MappingNode   => convertMappingNode(mapping)
    case sequence: Node.SequenceNode => convertSequenceNode(sequence)
    case kv: Node.KeyValueNode       => convertKeyValueNode(kv)

  private def convertMappingNode(node: Node.MappingNode): Seq[EventKind] =
    val events = node.mappings.map(convertNode(_)).flatten
    Seq(MappingStart()) ++ events ++ Seq(MappingEnd)

  private def convertSequenceNode(node: Node.SequenceNode): Seq[EventKind] =
    val events = node.nodes.map(convertNode(_)).flatten
    Seq(SequenceStart()) ++ events ++ Seq(SequenceEnd)

  private def convertKeyValueNode(node: Node.KeyValueNode): Seq[EventKind] =
    convertNode(node.key) ++ convertNode(node.value)

  private def convertScalarNode(node: Node.ScalarNode): Seq[EventKind] =
    Seq(Scalar(node.value))
