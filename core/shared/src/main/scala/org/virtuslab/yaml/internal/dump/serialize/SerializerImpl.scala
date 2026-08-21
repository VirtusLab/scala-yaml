package org.virtuslab.yaml.internal.dump.serialize

import scala.collection.mutable.Builder
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.internal.load.parse.EventKind
import org.virtuslab.yaml.internal.load.parse.EventKind._
import org.virtuslab.yaml.internal.load.parse.NodeEventMetadata

import scala.collection.immutable.ArraySeq

object SerializerImpl extends Serializer {
  override def toEvents(node: Node): Seq[EventKind] = {
    val builder = ArraySeq.newBuilder[EventKind]
    builder.addOne(DocumentStart)
    convertNode(node, builder)
    builder.addOne(DocumentEnd).result()
  }

  private def convertNode(node: Node, builder: Builder[EventKind, Seq[EventKind]]): Unit =
    builder.addOne(node match {
      case scalar: Node.ScalarNode =>
        new Scalar(scalar.value, metadata = new NodeEventMetadata(tag = new Some(scalar.tag)))
      case mapping: Node.MappingNode =>
        builder.addOne(MappingStart)
        val it = mapping.mappings.iterator
        while (it.hasNext) {
          val kv = it.next()
          convertNode(kv._1, builder)
          convertNode(kv._2, builder)
        }
        MappingEnd
      case sequence: Node.SequenceNode =>
        builder.addOne(SequenceStart)
        val it = sequence.nodes.iterator
        while (it.hasNext) convertNode(it.next(), builder)
        SequenceEnd
    })

  private val MappingStart  = new MappingStart()
  private val SequenceStart = new SequenceStart()
  private val DocumentStart = new DocumentStart()
  private val DocumentEnd   = new DocumentEnd()
}
