package org.virtuslab.internal.load.parse

import org.virtuslab.internal.load.reader.token.ScalarStyle

/** Valid sequence of events should obey following grammar
 * stream ::= STREAM-START document* STREAM-END
 * document ::= DOCUMENT-START node DOCUMENT-END
 * node ::= ALIAS | SCALAR | sequence | mapping  
 * sequence ::= SEQUENCE-START node* SEQUENCE-END 
 * mapping ::= MAPPING-START (node node)* MAPPING-END
  */

sealed trait Event
object Event:
  sealed trait Stream     extends Event
  case object StreamStart extends Stream
  case object StreamEnd   extends Stream

  sealed trait Document                               extends Event
  case class DocumentStart(explicit: Boolean = false) extends Document
  case class DocumentEnd(explicit: Boolean = false)   extends Document

  sealed trait Node                                    extends Event
  case class Scalar(value: String, style: ScalarStyle) extends Node
  case object Scalar:
    def apply(value: String): Scalar = value match // TODO it the same as in Token.Scalar
      case s""""$v"""" => Scalar(v, ScalarStyle.DoubleQuoted)
      case s"'$v'"     => Scalar(v, ScalarStyle.SingleQuoted)
      case v           => Scalar(v, ScalarStyle.Plain)

  sealed trait Sequence     extends Node
  case object SequenceStart extends Sequence
  case object SequenceEnd   extends Sequence

  sealed trait Mapping     extends Node
  case object MappingStart extends Mapping
  case object MappingEnd   extends Mapping
