package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.Position

/** 
 * 
 * Valid sequence of events should obey following grammar
 * stream ::= STREAM-START document* STREAM-END
 * document ::= DOCUMENT-START node DOCUMENT-END
 * node ::= ALIAS | SCALAR | sequence | mapping  
 * sequence ::= SEQUENCE-START node* SEQUENCE-END
 * mapping ::= MAPPING-START (node node)* MAPPING-END
 */
sealed trait Event:
  def pos: Option[Position]
object Event:
  sealed trait Stream                                  extends Event
  case class StreamStart(pos: Option[Position] = None) extends Stream
  case class StreamEnd(pos: Option[Position] = None)   extends Stream

  sealed trait Document extends Event
  final case class DocumentStart(pos: Option[Position] = None, explicit: Boolean = false)
      extends Document
  final case class DocumentEnd(pos: Option[Position] = None, explicit: Boolean = false)
      extends Document

  sealed trait Node extends Event
  final case class Scalar(
      value: String,
      style: ScalarStyle = ScalarStyle.Plain,
      pos: Option[Position] = None
  ) extends Node

  sealed trait Sequence                                  extends Node
  case class SequenceStart(pos: Option[Position] = None) extends Sequence
  case class SequenceEnd(pos: Option[Position] = None)   extends Sequence

  sealed trait Mapping                                      extends Node
  case class MappingStart(pos: Option[Position] = None)     extends Mapping
  case class MappingEnd(pos: Option[Position] = None)       extends Mapping
  case class FlowMappingStart(pos: Option[Position] = None) extends Mapping
  case class FlowMappingEnd(pos: Option[Position] = None)   extends Mapping
