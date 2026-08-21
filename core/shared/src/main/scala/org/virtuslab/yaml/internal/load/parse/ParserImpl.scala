package org.virtuslab.yaml.internal.load.parse

import scala.collection.mutable
import org.virtuslab.yaml.{CoreSchemaTag, CustomTag, ParseError, Tag, YamlError}
import org.virtuslab.yaml.internal.load.TagValue
import org.virtuslab.yaml.internal.load.reader.Tokenizer
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind

import scala.annotation.tailrec

private sealed abstract class Production
private object Production {
  case object ParseStreamStart      extends Production
  case object ParseStreamEnd        extends Production
  case object ParseDocumentStart    extends Production
  case object ParseDocumentEnd      extends Production
  case object ParseDocumentStartOpt extends Production

  case object ParseNode   extends Production
  case object ParseScalar extends Production

  case object ParseMappingEnd         extends Production
  case object ParseMappingEntry       extends Production
  case object ParseMappingValue       extends Production
  case object ParseMappingValueNode   extends Production
  case object ParseMappingSequenceEnd extends Production
  case object ParseMappingEntryOpt    extends Production

  case object ParseSequenceEnd      extends Production
  case object ParseSequenceEntry    extends Production
  case object ParseSequenceEntryOpt extends Production

  case object ParseFlowNode extends Production

  case object ParseFlowMappingEnd      extends Production
  case object ParseFlowMappingEntry    extends Production
  case object ParseFlowMappingEntryOpt extends Production
  case object ParseFlowMappingComma    extends Production

  case object ParseFlowSeqEnd       extends Production
  case object ParseFlowSeqEntry     extends Production
  case object ParseFlowSeqEntryOpt  extends Production
  case object ParseFlowSeqPairKey   extends Production
  case object ParseFlowSeqPairValue extends Production
  case object ParseFlowSeqComma     extends Production

  case class ReturnEvent(produceEvent: Token => Event) extends Production
}

/**
 * Parser takes a stream of [[Token]]s and produces a series of serialization [[Event]]s. Parsing can fail due to ill-formed input.
 * 
 * ParserImpl is using following productions:
 * 
 * ParseStreamStart              ::= (stream_start) ParseDocumentStart ParseDocumentStartOpt ParseStreamEnd
 * ParseStreamEnd                ::= (stream_end)
 *         
 * ParseDocumentStart            ::= (document_start) ParseNode ParseDocumentEnd
 * ParseDocumentEnd              ::= (document_end)
 * ParseDocumentStartOpt         ::= epsilon | ParseDocumentStart ParseDocumentStartOpt
 * 
 * ParseNode(indentLess=false)   ::= ParseMappingStart | ParseFlowMappingStart |
 *                                   ParseSequenceStart(indentLess) | ParseFlowSeqStart | ParseScalar
 * ParseScalar                   ::= scalar
 * 
 * ParseMappingStart             ::= mapping_start ParseMappingEntry ParseMappingEnd
 * ParseMappingEnd               ::= mapping_end
 * ParseMappingEntry             ::= mapping_key ParseScalar ParseMappingValue ParseMappingEntryOpt
 * ParseMappingValue             ::= mapping_value ParseNode(true)
 * ParseMappingEntryOpt          ::= epsilon | ParseMappingEntry
 * 
 * 
 * ParseSequenceStart(indentLess)::= (seq_start) ParseSequenceEntry ParseSequenceEnd(indentLess)
 * ParseSequenceEnd(indentLess)  ::= (seq_end)
 * ParseSequenceEntry            ::= seq_value ParseNode ParseSequenceEntryOpt
 * ParseSequenceEntryOpt         ::= epsilon | ParseSequenceEntry
 * 
 * ParseFlowNode                 ::= ParseFlowMappingStart | ParseFlowSeqStart | ParseScalar 
 * 
 * ParseFlowMappingStart         ::= flow_mapping_start ParseFlowMappingEntryOpt ParseFlowMappingEnd
 * ParseFlowMappingEnd           ::= flow_mapping_end
 * ParseFlowMappingEntry         ::= mapping_key ParseScalar ParseMappingValue 
 * ParseFlowMappingEntryOpt      ::= epsilon | ParseFlowMappingEntry
 * ParseFlowMappingComma         ::= epsilon | comma ParseFlowMappingEntryOpt
 * 
 * ParseFlowSeqStart             ::= flow_seq_start ParseFlowSeqEntryOpt ParseFlowSeqEnd
 * ParseFlowSeqEnd               ::= flow_seq_end
 * ParseFlowSeqEntry             ::= (ParseFlowNode | ParseFlowPairKey) ParseFlowSeqComma
 * ParseFlowSeqEntryOpt          ::= epsilon | ParseFlowSeqEntry
 * ParseFlowSeqComma             ::= epsilon | comma ParseFlowSeqEntryOpt
 * 
 * 
 * ParseFlowPairKey              ::= mapping_key ParseFlowNode ParseFlowPairValue
 * ParseFlowPairValue            ::= mapping_value ParseFlowNode
 * 
*/
final class ParserImpl private (in: Tokenizer) extends Parser {
  import Production._

  private val productions = mutable.ArrayDeque[Production](ParseStreamStart)
  // Primary ('!') and secondary ('!!') tags, if not overwritten by tag directive, have some predefined defaults
  private val defaultDirectives = Map("!" -> "!", "!!" -> "tag:yaml.org,2002:")
  private val directives        = defaultDirectives.to(mutable.Map)

  def getEvents(): Either[YamlError, List[Event]] =
    try {
      val events = new mutable.ListBuffer[Event]
      while (productions.length > 0) events.append(getNextEventImpl())
      new Right(events.toList)
    } catch {
      case err: YamlError => new Left(err)
    }

  override def getNextEvent(): Either[YamlError, Event] =
    try
      new Right({
        if (productions.length > 0) getNextEventImpl()
        else Event.streamEnd
      })
    catch {
      case err: YamlError => new Left(err)
    }

  private def clearDirectives(): Unit = {
    directives.clear()
    directives.addAll(defaultDirectives)
  }

  private def parseStreamStart(token: Token) = {
    productions.prepend(ParseStreamEnd).prepend(ParseDocumentStartOpt)
    Event(EventKind.StreamStart, token.range)
  }

  private def parseDocumentStart(token: Token) = {
    productions.prepend(ParseDocumentEnd).prepend(ParseNode)
    Event(
      new EventKind.DocumentStart(explicit = {
        (token.kind eq TokenKind.DocumentStart) && {
          in.popToken()
          true
        }
      }),
      token.range
    )
  }

  private def parseDocumentStartOpt(token: Token) = {
    token.kind match {
      case td: TokenKind.TagDirective =>
        directives.update(td.handle.value, td.prefix.value)
        in.popToken()
        productions.prepend(ParseDocumentStartOpt) // call self once again
      case _: TokenKind.DocumentStart.type =>
        productions.prepend(ParseDocumentStartOpt).prepend(ParseDocumentStart)
      case _: TokenKind.MappingStart.type | _: TokenKind.Scalar | _: TokenKind.SequenceStart.type |
          _: TokenKind.FlowMappingStart.type | _: TokenKind.FlowSequenceStart.type |
          _: TokenKind.Anchor | _: TokenKind.Tag =>
        productions.prepend(ParseDocumentStartOpt).prepend(ParseDocumentStart)
      case _ =>
    }
    getNextEventImpl()
  }

  private def parseDocumentEnd(token: Token) = {
    clearDirectives()
    Event(
      EventKind.DocumentEnd(explicit = {
        (token.kind eq TokenKind.DocumentEnd) && {
          in.popToken()
          true
        }
      }),
      token.range
    )
  }

  private def parseMappingEnd(token: Token) =
    if (token.kind eq TokenKind.BlockEnd) {
      in.popToken()
      Event(EventKind.MappingEnd, token.range)
    } else throw ParseError.from(TokenKind.BlockEnd, token)

  private def parseMappingEntry(token: Token) =
    if (token.kind eq TokenKind.MappingKey) {
      in.popToken()
      productions.prepend(ParseMappingEntryOpt).prepend(ParseMappingValue).prepend(ParseScalar)
      getNextEventImpl()
    } else throw ParseError.from(TokenKind.MappingKey, token)

  private def parseMappingValue(token: Token) =
    if (token.kind eq TokenKind.MappingValue) {
      in.popToken()
      productions.prepend(ParseMappingValueNode)
      getNextEventImpl()
    } else throw ParseError.from(TokenKind.MappingValue, token)

  private def parseMappingValueNode(token: Token) =
    if (token.kind eq TokenKind.SequenceValue) {
      productions.prepend(ParseMappingSequenceEnd).prepend(ParseSequenceEntry)
      Event(new EventKind.SequenceStart(), token.range)
    } else parseNode(token)

  private def parseMappingSequenceEnd(token: Token) =
    Event(EventKind.SequenceEnd, token.range)

  private def parseMappingEntryOpt(token: Token) = {
    if (token.kind eq TokenKind.MappingKey) productions.prepend(ParseMappingEntry)
    getNextEventImpl()
  }

  private def parseSequenceEnd(token: Token) =
    if (token.kind eq TokenKind.BlockEnd) {
      in.popToken()
      Event(EventKind.SequenceEnd, token.range)
    } else throw ParseError.from(TokenKind.BlockEnd, token)

  private def parseSequenceEntry(token: Token) =
    if (token.kind eq TokenKind.SequenceValue) {
      in.popToken()
      productions.prepend(ParseSequenceEntryOpt).prepend(ParseNode)
      getNextEventImpl()
    } else throw ParseError.from(TokenKind.SequenceValue, token)

  private def parseSequenceEntryOpt(token: Token) = {
    if (token.kind eq TokenKind.SequenceValue) productions.prepend(ParseSequenceEntry)
    getNextEventImpl()
  }

  private def parseFlowMappingEnd(token: Token) =
    if (token.kind eq TokenKind.FlowMappingEnd) {
      in.popToken()
      Event(EventKind.MappingEnd, token.range)
    } else throw ParseError.from(TokenKind.FlowMappingEnd, token)

  private def parseFlowMappingEntry(token: Token) = {
    if (token.kind eq TokenKind.MappingKey) {
      in.popToken()
      productions.prepend(ParseFlowMappingComma).prepend(ParseMappingValue).prepend(ParseScalar)
    }
    getNextEventImpl()
  }

  private def parseFlowMappingEntryOpt(token: Token) =
    token.kind match {
      case _: TokenKind.Scalar | _: TokenKind.Anchor =>
        productions
          .prepend(ParseFlowMappingEntry)
          .prepend(ParseFlowMappingComma)
          .prepend(ParseFlowNode)
        parseFlowNode(token)
      case k =>
        if (k eq TokenKind.MappingKey) { // flow mapping start right after flow mapping start{>>{
          productions.prepend(ParseFlowMappingEntry)
        } else if (
          (k eq TokenKind.FlowMappingStart) || // flow sequence start right after flow mapping start{>>[
          (k eq TokenKind.FlowSequenceStart)
        ) {
          productions.prepend(ParseFlowNode)
        }
        getNextEventImpl()
    }

  private def parseFlowMappingComma(token: Token) = {
    if (token.kind eq TokenKind.Comma) {
      in.popToken()
      productions.prepend(ParseFlowMappingEntryOpt)
    }
    getNextEventImpl()
  }

  private def parseFlowSeqEnd(token: Token) =
    if (token.kind eq TokenKind.FlowSequenceEnd) {
      in.popToken()
      Event(EventKind.SequenceEnd, token.range)
    } else throw ParseError.from(TokenKind.FlowSequenceEnd, token)

  private def parseFlowSeqEntry(token: Token) = {
    productions.prepend(ParseFlowSeqComma)
    if (token.kind eq TokenKind.MappingKey) {
      productions.prepend(ParseFlowSeqPairKey)
      Event(new EventKind.MappingStart(), token.range)
    } else {
      productions.prepend(ParseFlowNode)
      getNextEventImpl()
    }
  }

  private def parseFlowSeqEntryOpt(token: Token) = {
    token.kind match {
      case _: TokenKind.FlowMappingStart.type | _: TokenKind.FlowSequenceStart.type |
          _: TokenKind.Scalar | _: TokenKind.Alias | _: TokenKind.Anchor |
          _: TokenKind.MappingKey.type =>
        productions.prepend(ParseFlowSeqEntry)
      case _ =>
    }
    getNextEventImpl()
  }

  private def parseFlowPairKey(token: Token) =
    if (token.kind eq TokenKind.MappingKey) {
      in.popToken()
      productions
        .prepend(new ReturnEvent(t => Event(EventKind.MappingEnd, t.range)))
        .prepend(ParseFlowSeqPairValue)
        .prepend(ParseFlowNode)
      getNextEventImpl()
    } else throw ParseError.from(TokenKind.MappingKey, token)

  private def parseFlowPairValue(token: Token) =
    if (token.kind eq TokenKind.MappingValue) {
      in.popToken()
      productions.prepend(ParseFlowNode)
      getNextEventImpl()
    } else throw ParseError.from(TokenKind.MappingValue, token)

  private def parseFlowSeqComma(token: Token) = {
    if (token.kind eq TokenKind.Comma) {
      in.popToken()
      productions.prepend(ParseFlowSeqEntryOpt)
    }
    getNextEventImpl()
  }

  private def parseScalar(token: Token) =
    parseNodeAttributes(token) match {
      case (metadata, nextToken) =>
        nextToken.kind match {
          case TokenKind.Scalar(value, style) =>
            in.popToken()
            Event(new EventKind.Scalar(value, style, metadata), token.range)
          case TokenKind.Alias(alias) =>
            if (metadata.anchor.isEmpty) {
              in.popToken()
              Event(new EventKind.Alias(new Anchor(alias)), nextToken.range)
            } else throw ParseError.from("Alias cannot have an anchor", nextToken)
          case _ =>
            throw ParseError.from(TokenKind.Scalar.toString, token)
        }
    }

  private def parseFlowNode(token: Token) =
    parseNode(token, couldParseBlockCollection = false)

  private def parseNode(
      token: Token,
      couldParseBlockCollection: Boolean = true
  ): Event = {
    val (metadata, nextToken) = parseNodeAttributes(token)
    nextToken.kind match {
      case a: TokenKind.Alias =>
        if (metadata.anchor.isEmpty) {
          in.popToken()
          Event(new EventKind.Alias(new Anchor(a.value)), nextToken.range)
        } else throw ParseError.from("Alias cannot have an anchor", nextToken)
      case _: TokenKind.MappingStart.type if couldParseBlockCollection =>
        in.popToken()
        productions.prepend(ParseMappingEnd).prepend(ParseMappingEntry)
        Event(new EventKind.MappingStart(metadata), nextToken.range)
      case _: TokenKind.SequenceStart.type if couldParseBlockCollection =>
        in.popToken()
        productions.prepend(ParseSequenceEnd).prepend(ParseSequenceEntry)
        Event(new EventKind.SequenceStart(metadata), nextToken.range)
      case _: TokenKind.FlowMappingStart.type =>
        in.popToken()
        productions.prepend(ParseFlowMappingEnd).prepend(ParseFlowMappingEntryOpt)
        Event(new EventKind.MappingStart(metadata), nextToken.range)
      case _: TokenKind.FlowSequenceStart.type =>
        in.popToken()
        productions.prepend(ParseFlowSeqEnd).prepend(ParseFlowSeqEntryOpt)
        Event(new EventKind.SequenceStart(metadata), nextToken.range)
      case s: TokenKind.Scalar =>
        in.popToken()
        Event(new EventKind.Scalar(s.value, s.scalarStyle, metadata), nextToken.range)
      case _ =>
        Event(
          new EventKind.Scalar("", ScalarStyle.Plain, metadata.withTag(Tag.nullTag)),
          nextToken.range
        )
    }
  }

  @tailrec
  private def parseNodeAttributes(
      token: Token,
      metadata: NodeEventMetadata = NodeEventMetadata.empty
  ): (NodeEventMetadata, Token) = token.kind match {
    case a: TokenKind.Anchor =>
      in.popToken()
      parseNodeAttributes(in.peekTokenUnsafe(), metadata.withAnchor(new Anchor(a.value)))
    case t: TokenKind.Tag =>
      in.popToken()
      t.value match {
        case _: TagValue.NonSpecific.type =>
          parseNodeAttributes(in.peekTokenUnsafe(), metadata)
        case v: TagValue.Verbatim =>
          parseNodeAttributes(in.peekTokenUnsafe(), metadata.withTag(new CustomTag(v.value)))
        case s: TagValue.Shorthand =>
          val handleKey = s.handle.value
          directives.get(handleKey) match {
            case Some(prefix) =>
              val tagValue = prefix.concat(s.rest)
              val tag =
                if (Tag.coreSchemaValues.contains(tagValue)) new CoreSchemaTag(tagValue)
                else new CustomTag(tagValue)
              parseNodeAttributes(in.peekTokenUnsafe(), metadata.withTag(tag))
            case _ =>
              throw ParseError.NoRegisteredTagDirective(handleKey, token)
          }
      }
    case _ => (metadata, token)
  }

  private def getNextEventImpl(): Event = {
    val token = in.peekTokenUnsafe()
    val p     = productions.removeHead()
    if (p eq ParseStreamStart) parseStreamStart(token)
    else if (p eq ParseStreamEnd) Event(EventKind.StreamEnd, token.range)
    else if (p eq ParseDocumentStart) parseDocumentStart(token)
    else if (p eq ParseDocumentEnd) parseDocumentEnd(token)
    else if (p eq ParseDocumentStartOpt) parseDocumentStartOpt(token)
    else if (p eq ParseNode) parseNode(token)
    else if (p eq ParseScalar) parseScalar(token)
    else if (p eq ParseMappingEnd) parseMappingEnd(token)
    else if (p eq ParseMappingEntry) parseMappingEntry(token)
    else if (p eq ParseMappingValue) parseMappingValue(token)
    else if (p eq ParseMappingValueNode) parseMappingValueNode(token)
    else if (p eq ParseMappingSequenceEnd) parseMappingSequenceEnd(token)
    else if (p eq ParseMappingEntryOpt) parseMappingEntryOpt(token)
    else if (p eq ParseSequenceEnd) parseSequenceEnd(token)
    else if (p eq ParseSequenceEntry) parseSequenceEntry(token)
    else if (p eq ParseSequenceEntryOpt) parseSequenceEntryOpt(token)
    else if (p eq ParseFlowNode) parseFlowNode(token)
    else if (p eq ParseFlowMappingEnd) parseFlowMappingEnd(token)
    else if (p eq ParseFlowMappingEntry) parseFlowMappingEntry(token)
    else if (p eq ParseFlowMappingEntryOpt) parseFlowMappingEntryOpt(token)
    else if (p eq ParseFlowMappingComma) parseFlowMappingComma(token)
    else if (p eq ParseFlowSeqEnd) parseFlowSeqEnd(token)
    else if (p eq ParseFlowSeqEntry) parseFlowSeqEntry(token)
    else if (p eq ParseFlowSeqEntryOpt) parseFlowSeqEntryOpt(token)
    else if (p eq ParseFlowSeqComma) parseFlowSeqComma(token)
    else if (p eq ParseFlowSeqPairKey) parseFlowPairKey(token)
    else if (p eq ParseFlowSeqPairValue) parseFlowPairValue(token)
    else p.asInstanceOf[ReturnEvent].produceEvent(token)
  }
}

object ParserImpl {
  def apply(in: Tokenizer): ParserImpl = new ParserImpl(in)
}
