package org.virtuslab.yaml.internal.dump.serialize

import org.virtuslab.yaml.Node
import org.virtuslab.yaml.internal.load.parse.Event
import org.virtuslab.yaml.internal.load.reader.Position

object SerializerImpl extends Serializer:
  override def toEvents(node: Node): Seq[Event] =
    Seq(Event.DocumentStart()) ++ convertNode(node) ++ Seq(
      Event.DocumentEnd()
    )

  private def convertNode(node: Node) = node match
    case scalar: Node.ScalarNode     => convertScalarNode(scalar)
    case mapping: Node.MappingNode   => convertMappingNode(mapping)
    case sequence: Node.SequenceNode => convertSequenceNode(sequence)
    case kv: Node.KeyValueNode       => convertKeyValueNode(kv)

  private def convertMappingNode(node: Node.MappingNode): Seq[Event] =
    val events = node.mappings.map(convertNode(_)).flatten
    Seq(Event.MappingStart()) ++ events ++ Seq(Event.MappingEnd())

  private def convertSequenceNode(node: Node.SequenceNode): Seq[Event] =
    val events = node.nodes.map(convertNode(_)).flatten
    Seq(Event.SequenceStart()) ++ events ++ Seq(Event.SequenceEnd())

  private def convertKeyValueNode(node: Node.KeyValueNode): Seq[Event] =
    Seq(Event.Scalar(node.key.value)) ++ convertNode(node.value)

  private def convertScalarNode(node: Node.ScalarNode): Seq[Event] =
    Seq(Event.Scalar(node.value))
