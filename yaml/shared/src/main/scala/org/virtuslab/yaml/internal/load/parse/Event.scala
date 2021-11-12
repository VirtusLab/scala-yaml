package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Range
import org.virtuslab.yaml.Tag
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

/** 
 * 
 * Valid sequence of events should obey following grammar
 * stream ::= STREAM-START document* STREAM-END
 * document ::= DOCUMENT-START node DOCUMENT-END
 * node ::= ALIAS | SCALAR | sequence | mapping  
 * sequence ::= SEQUENCE-START node* SEQUENCE-END
 * mapping ::= MAPPING-START (node node)* MAPPING-END
 */
final case class Event(kind: EventKind, pos: Option[Range])
object Event:
  import EventKind.*

  val streamStart = Event(StreamStart, None)
  val streamEnd   = Event(StreamEnd, None)

  def apply(kind: EventKind, pos: Range): Event = Event(kind, Some(pos))

enum EventKind:
  case StreamStart
  case StreamEnd
  case DocumentStart(explicit: Boolean = false)
  case DocumentEnd(explicit: Boolean = false)

  case Alias(id: Anchor)
  case Scalar(
      value: String,
      style: ScalarStyle = ScalarStyle.Plain,
      metadata: NodeEventMetadata = NodeEventMetadata.empty
  )

  case SequenceStart(metadata: NodeEventMetadata = NodeEventMetadata.empty)
  case SequenceEnd

  case MappingStart(metadata: NodeEventMetadata = NodeEventMetadata.empty)
  case MappingEnd

  case FlowMappingStart(metadata: NodeEventMetadata = NodeEventMetadata.empty)
  case FlowMappingEnd

/**
 * Carries additional information about event which represents YAML node (scalar, start of mapping or sequence).
 * This could be:
 * - anchor
 * - tags (not yet supported)
 */
final case class NodeEventMetadata(
    anchor: Option[Anchor] = None,
    tag: Option[Tag] = None
):
  def withAnchor(anchor: Anchor) = this.copy(anchor = Some(anchor))
  def withTag(tag: Tag)          = this.copy(tag = Some(tag))
end NodeEventMetadata

object NodeEventMetadata:
  final val empty                              = NodeEventMetadata()
  def apply(anchor: Anchor): NodeEventMetadata = NodeEventMetadata(anchor = Some(anchor))
  @scala.annotation.targetName("applyForTag")
  def apply(tag: Tag): NodeEventMetadata = NodeEventMetadata(tag = Some(tag))

opaque type Anchor = String
object Anchor:
  def apply(anchor: String): Anchor = anchor
