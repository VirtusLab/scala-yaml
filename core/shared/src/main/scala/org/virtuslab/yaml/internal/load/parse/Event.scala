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
object Event {
  import EventKind._

  val streamStart = Event(StreamStart, None)
  val streamEnd   = Event(StreamEnd, None)

  def apply(kind: EventKind, pos: Range): Event = new Event(kind, new Some(pos))
}

sealed abstract class EventKind
object EventKind {
  case object StreamStart                             extends EventKind
  case object StreamEnd                               extends EventKind
  case class DocumentStart(explicit: Boolean = false) extends EventKind
  case class DocumentEnd(explicit: Boolean = false)   extends EventKind

  case class Alias(id: Anchor) extends EventKind
  case class Scalar(
      value: String,
      style: ScalarStyle = ScalarStyle.Plain,
      metadata: NodeEventMetadata = NodeEventMetadata.empty
  ) extends EventKind

  case class SequenceStart(metadata: NodeEventMetadata = NodeEventMetadata.empty) extends EventKind
  case object SequenceEnd                                                         extends EventKind

  case class MappingStart(metadata: NodeEventMetadata = NodeEventMetadata.empty) extends EventKind
  case object MappingEnd                                                         extends EventKind
}

/**
 * Carries additional information about event which represents YAML node (scalar, start of mapping or sequence).
 * This could be:
 * - anchor
 * - tags (not yet supported)
 */
final case class NodeEventMetadata(
    anchor: Option[Anchor] = None,
    tag: Option[Tag] = None
) {
  def withAnchor(anchor: Anchor) = this.copy(anchor = new Some(anchor))
  def withTag(tag: Tag)          = this.copy(tag = new Some(tag))
}

object NodeEventMetadata {
  final val empty                              = new NodeEventMetadata()
  def apply(anchor: Anchor): NodeEventMetadata = new NodeEventMetadata(anchor = new Some(anchor))
  def apply(anchor: Anchor, tag: Tag): NodeEventMetadata =
    new NodeEventMetadata(anchor = Some(anchor), tag = Some(tag))
  def apply(tag: Tag): NodeEventMetadata = new NodeEventMetadata(tag = new Some(tag))
}

class Anchor(val anchor: String) extends AnyVal
object Anchor {
  def apply(anchor: String): Anchor = new Anchor(anchor)
}
