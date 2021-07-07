package org.virtuslab.internal.load.parse

import org.virtuslab.internal.load.YamlReader

sealed trait ParserError

case class ParserCtx(
    state: ParserState
)

trait Parser:
  def getEvents(in: YamlReader, ctx: ParserCtx): Either[ParserError, Seq[Event]]

/** Valid sequence of events should obey following grammar so parser states
  * should mirror that grammar stream ::= STREAM-START document* STREAM-END
  * document ::= DOCUMENT-START node DOCUMENT-END node ::= ALIAS | SCALAR |
  * sequence | mapping sequence ::= SEQUENCE-START node* SEQUENCE-END mapping
  * ::= MAPPING-START (node node)* MAPPING-END
  */
sealed trait ParserState

/** Parser in state X expects to parse X event
  */
object ParserState:
  case object EndState extends ParserState
  case object StreamStart extends ParserState
  case object StreamEnd extends ParserState
  case object DocumentStart extends ParserState
  case object DocumentEnd extends ParserState
  case object Node extends ParserState
  case object Mapping extends ParserState
  case object SequenceStart extends ParserState
  case object SequenceEnd extends ParserState
  case object FirstKey extends ParserState
  case object Key extends ParserState
  case object Value extends ParserState
  case object Scalar extends ParserState
