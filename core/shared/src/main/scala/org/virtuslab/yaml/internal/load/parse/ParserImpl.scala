package org.virtuslab.yaml.internal.load.parse

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.virtuslab.yaml.CoreSchemaTag
import org.virtuslab.yaml.CustomTag
import org.virtuslab.yaml.ParseError
import org.virtuslab.yaml.Range
import org.virtuslab.yaml.Tag
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.TagHandle
import org.virtuslab.yaml.internal.load.TagValue
import org.virtuslab.yaml.internal.load.reader.Tokenizer
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind

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

  private[yaml] def getEvents(): Either[YamlError, List[Event]] = {
    @tailrec
    def loop(events: mutable.ArrayDeque[Event]): Either[YamlError, List[Event]] = {
      getNextEvent() match {
        case Right(event) =>
          if (event.kind != EventKind.StreamEnd) loop(events.append(event))
          else Right(events.append(event).toList)
        case Left(err) => Left(err)
      }
    }
    loop(new mutable.ArrayDeque())
  }

  override def getNextEvent(): Either[YamlError, Event] =
    if (productions.size > 0) getNextEventImpl()
    else Right(Event.streamEnd)

  private def clearDirectives(): Unit = {
    directives.clear()
    directives.addAll(defaultDirectives)
  }

  private def parseStreamStart(token: Token) = {
    productions.prependAll(ParseDocumentStartOpt :: ParseStreamEnd :: Nil)
    Right(Event(EventKind.StreamStart, token.range))
  }

  private def parseDocumentStart(token: Token) = token.kind match {
    case TokenKind.DocumentStart =>
      in.popToken()
      productions.prependAll(ParseNode :: ParseDocumentEnd :: Nil)
      Right(Event(EventKind.DocumentStart(explicit = true), token.range))
    case _ =>
      productions.prependAll(ParseNode :: ParseDocumentEnd :: Nil)
      Right(Event(EventKind.DocumentStart(explicit = false), token.range))
  }

  private def parseDocumentStartOpt(token: Token) = token.kind match {
    case TokenKind.TagDirective(handle, prefix) =>
      directives.update(handle.value, prefix.value)
      in.popToken()
      productions.prepend(ParseDocumentStartOpt) // call self once again
      getNextEventImpl()
    case TokenKind.DocumentStart =>
      productions.prependAll(ParseDocumentStart :: ParseDocumentStartOpt :: Nil)
      getNextEventImpl()
    case TokenKind.MappingStart | TokenKind.Scalar(_, _) | TokenKind.SequenceStart |
        TokenKind.FlowMappingStart | TokenKind.FlowSequenceStart | _: TokenKind.Anchor |
        _: TokenKind.Tag =>
      productions.prependAll(ParseDocumentStart :: ParseDocumentStartOpt :: Nil)
      getNextEventImpl()
    case _ =>
      getNextEventImpl()
  }

  private def parseDocumentEnd(token: Token) = {
    clearDirectives()
    token.kind match {
      case TokenKind.DocumentEnd =>
        in.popToken()
        Right(Event(EventKind.DocumentEnd(true), token.range))
      case _ =>
        Right(Event(EventKind.DocumentEnd(false), token.range))
    }
  }

  private def parseMappingEnd(token: Token) = token.kind match {
    case TokenKind.BlockEnd =>
      in.popToken()
      Right(Event(EventKind.MappingEnd, token.range))
    case _ =>
      Left(ParseError.from(TokenKind.BlockEnd, token))
  }

  private def parseMappingEntry(token: Token) = token.kind match {
    case TokenKind.MappingKey =>
      in.popToken()
      productions.prependAll(ParseScalar :: ParseMappingValue :: ParseMappingEntryOpt :: Nil)
      getNextEventImpl()
    case _ =>
      Left(ParseError.from(TokenKind.MappingKey, token))
  }

  private def parseMappingValue(token: Token) = token.kind match {
    case TokenKind.MappingValue =>
      in.popToken()
      productions.prependAll(ParseMappingValueNode :: Nil)
      getNextEventImpl()
    case _ => Left(ParseError.from(TokenKind.MappingValue, token))
  }

  private def parseMappingValueNode(token: Token) = token.kind match {
    case TokenKind.SequenceValue =>
      productions.prependAll(ParseSequenceEntry :: ParseMappingSequenceEnd :: Nil)
      Right(Event(EventKind.SequenceStart(), token.range))
    case _ => parseNode(token)
  }

  private def parseMappingSequenceEnd(token: Token) =
    Right(Event(EventKind.SequenceEnd, token.range))

  private def parseMappingEntryOpt(token: Token) = token.kind match {
    case TokenKind.MappingKey =>
      productions.prependAll(ParseMappingEntry :: Nil)
      getNextEventImpl()
    case _ =>
      getNextEventImpl()
  }

  private def parseSequenceEnd(token: Token) = token.kind match {
    case TokenKind.BlockEnd =>
      in.popToken()
      Right(Event(EventKind.SequenceEnd, token.range))
    case _ =>
      Left(ParseError.from(TokenKind.BlockEnd, token))
  }

  private def parseSequenceEntry(token: Token) = token.kind match {
    case TokenKind.SequenceValue =>
      in.popToken()
      productions.prependAll(ParseNode :: ParseSequenceEntryOpt :: Nil)
      getNextEventImpl()
    case _ => Left(ParseError.from(TokenKind.SequenceValue, token))
  }

  private def parseSequenceEntryOpt(token: Token) = token.kind match {
    case TokenKind.SequenceValue =>
      productions.prependAll(ParseSequenceEntry :: Nil)
      getNextEventImpl()
    case _ =>
      getNextEventImpl()
  }

  private def parseFlowMappingStart(token: Token) = token.kind match {
    case TokenKind.FlowMappingStart =>
      in.popToken()
      productions.prependAll(ParseFlowMappingEntryOpt :: ParseFlowMappingEnd :: Nil)
      Right(Event(EventKind.MappingStart(), token.range))
    case _ =>
      Left(ParseError.from(TokenKind.FlowMappingStart, token))
  }

  private def parseFlowMappingEnd(token: Token) = token.kind match {
    case TokenKind.FlowMappingEnd =>
      in.popToken()
      Right(Event(EventKind.MappingEnd, token.range))
    case _ =>
      Left(ParseError.from(TokenKind.FlowMappingEnd, token))
  }

  private def parseFlowMappingEntry(token: Token) = token.kind match {
    case TokenKind.MappingKey =>
      in.popToken()
      productions.prependAll(ParseScalar :: ParseMappingValue :: ParseFlowMappingComma :: Nil)
      getNextEventImpl()
    case _ =>
      getNextEventImpl()
  }

  private def parseFlowMappingEntryOpt(token: Token) = token.kind match {
    case TokenKind.MappingKey =>
      productions.prependAll(ParseFlowMappingEntry :: Nil)
      getNextEventImpl()
    // flow mapping start right after flow mapping start{>>{
    case TokenKind.FlowMappingStart =>
      productions.prependAll(ParseFlowNode :: Nil)
      getNextEventImpl()
    // flow sequence start right after flow mapping start{>>[
    case TokenKind.FlowSequenceStart =>
      productions.prependAll(ParseFlowNode :: Nil)
      getNextEventImpl()
    case TokenKind.Scalar(_, _) | _: TokenKind.Anchor =>
      productions.prependAll(
        ParseFlowNode :: ParseFlowMappingComma :: ParseFlowMappingEntry :: Nil
      )
      parseFlowNode(token)
    case _ =>
      getNextEventImpl()
  }

  private def parseFlowMappingValue(token: Token) = token.kind match {
    case TokenKind.MappingValue =>
      in.popToken()
      productions.prependAll(ParseFlowNode :: Nil)
      getNextEventImpl()
    case _ => Left(ParseError.from(TokenKind.MappingValue, token))
  }

  private def parseFlowMappingComma(token: Token) = token.kind match {
    case TokenKind.Comma =>
      in.popToken()
      productions.prependAll(ParseFlowMappingEntryOpt :: Nil)
      getNextEventImpl()
    case _ =>
      getNextEventImpl()
  }

  private def parseFlowSeqEnd(token: Token) = token.kind match {
    case TokenKind.FlowSequenceEnd =>
      in.popToken()
      Right(Event(EventKind.SequenceEnd, token.range))
    case _ =>
      Left(ParseError.from(TokenKind.FlowSequenceEnd, token))
  }

  private def parseFlowSeqEntry(token: Token) = token.kind match {
    case TokenKind.MappingKey =>
      productions.prependAll(ParseFlowSeqPairKey :: ParseFlowSeqComma :: Nil)
      Right(Event(EventKind.MappingStart(), token.range))
    case _ =>
      productions.prependAll(ParseFlowNode :: ParseFlowSeqComma :: Nil)
      getNextEventImpl()
  }

  private def parseFlowSeqEntryOpt(token: Token) = token.kind match {
    case TokenKind.FlowMappingStart | TokenKind.FlowSequenceStart | _: TokenKind.Scalar |
        _: TokenKind.Alias | _: TokenKind.Anchor | TokenKind.MappingKey =>
      productions.prependAll(ParseFlowSeqEntry :: Nil)
      getNextEventImpl()
    case _ =>
      getNextEventImpl()
  }

  private def parseFlowPairKey(token: Token) = token.kind match {
    case TokenKind.MappingKey =>
      in.popToken()
      productions.prependAll(
        ParseFlowNode :: ParseFlowSeqPairValue :: ReturnEvent(t =>
          Event(EventKind.MappingEnd, t.range)
        ) :: Nil
      )
      getNextEventImpl()
    case _ =>
      Left(ParseError.from(TokenKind.MappingKey, token))
  }

  private def parseFlowPairValue(token: Token) = token.kind match {
    case TokenKind.MappingValue =>
      in.popToken()
      productions.prependAll(ParseFlowNode :: Nil)
      getNextEventImpl()
    case _ =>
      Left(ParseError.from(TokenKind.MappingValue, token))
  }

  private def parseFlowSeqComma(token: Token) = token.kind match {
    case TokenKind.Comma =>
      in.popToken()
      productions.prependAll(ParseFlowSeqEntryOpt :: Nil)
      getNextEventImpl()
    case _ =>
      getNextEventImpl()
  }

  private def parseScalar(token: Token) =
    parseNodeAttributes(Right(token)).flatMap { case (metadata, nextToken) =>
      nextToken.kind match {
        case TokenKind.Scalar(value, style) =>
          in.popToken()
          Right(Event(EventKind.Scalar(value, style, metadata), token.range))
        case TokenKind.Alias(alias) =>
          if (metadata.anchor.isDefined)
            Left(ParseError.from("Alias cannot have an anchor", nextToken))
          else {
            in.popToken()
            Right(Event(EventKind.Alias(Anchor(alias)), nextToken.range))
          }
        case _ =>
          Left(ParseError.from(TokenKind.Scalar.toString, token))
      }
    }

  private def parseFlowNode(token: Token) =
    parseNode(token, couldParseBlockCollection = false)

  private def parseNode(
      token: Token,
      couldParseBlockCollection: Boolean = true
  ): Either[YamlError, Event] =
    parseNodeAttributes(Right(token)).flatMap { case (metadata, nextToken) =>
      nextToken.kind match {
        case TokenKind.Alias(alias) =>
          if (metadata.anchor.isDefined)
            Left(ParseError.from("Alias cannot have an anchor", nextToken))
          else {
            in.popToken()
            Right(Event(EventKind.Alias(Anchor(alias)), nextToken.range))
          }
        case TokenKind.MappingStart if couldParseBlockCollection =>
          in.popToken()
          productions.prependAll(ParseMappingEntry :: ParseMappingEnd :: Nil)
          Right(Event(EventKind.MappingStart(metadata), nextToken.range))
        case TokenKind.SequenceStart if couldParseBlockCollection =>
          in.popToken()
          productions.prependAll(ParseSequenceEntry :: ParseSequenceEnd :: Nil)
          Right(Event(EventKind.SequenceStart(metadata), nextToken.range))
        case TokenKind.FlowMappingStart =>
          in.popToken()
          productions.prependAll(ParseFlowMappingEntryOpt :: ParseFlowMappingEnd :: Nil)
          Right(Event(EventKind.MappingStart(metadata), nextToken.range))
        case TokenKind.FlowSequenceStart =>
          in.popToken()
          productions.prependAll(ParseFlowSeqEntryOpt :: ParseFlowSeqEnd :: Nil)
          Right(Event(EventKind.SequenceStart(metadata), nextToken.range))
        case TokenKind.Scalar(value, style) =>
          in.popToken()
          Right(Event(EventKind.Scalar(value, style, metadata), nextToken.range))
        case _ =>
          Right(
            Event(
              EventKind.Scalar("", ScalarStyle.Plain, metadata.withTag(Tag.nullTag)),
              nextToken.range
            )
          )
      }
    }

  private def parseNodeAttributes(
      tokenE: Either[YamlError, Token],
      metadata: NodeEventMetadata = NodeEventMetadata.empty
  ): Either[YamlError, (NodeEventMetadata, Token)] =
    tokenE.flatMap { token =>
      token.kind match {
        case TokenKind.Anchor(value) =>
          in.popToken()
          parseNodeAttributes(in.peekToken(), metadata.withAnchor(Anchor(value)))
        case TokenKind.Tag(value) =>
          in.popToken()
          value match {
            case TagValue.NonSpecific =>
              parseNodeAttributes(in.peekToken(), metadata)
            case TagValue.Verbatim(value) =>
              parseNodeAttributes(in.peekToken(), metadata.withTag(CustomTag(value)))
            case TagValue.Shorthand(handle, suffix) =>
              val handleKey = handle.value
              directives.get(handleKey) match {
                case Some(prefix) =>
                  val tagValue = s"$prefix$suffix"
                  val tag =
                    if (Tag.coreSchemaValues.contains(tagValue)) CoreSchemaTag(tagValue)
                    else CustomTag(tagValue)
                  parseNodeAttributes(in.peekToken(), metadata.withTag(tag))
                case None =>
                  Left(ParseError.NoRegisteredTagDirective(handleKey, token))
              }
          }
        case _ => Right(metadata, token)
      }
    }

  private def getNextEventImpl(): Either[YamlError, Event] =
    in.peekToken().flatMap { token =>
      productions.removeHead() match {
        case ParseStreamStart      => parseStreamStart(token)
        case ParseStreamEnd        => Right(Event(EventKind.StreamEnd, token.range))
        case ParseDocumentStart    => parseDocumentStart(token)
        case ParseDocumentEnd      => parseDocumentEnd(token)
        case ParseDocumentStartOpt => parseDocumentStartOpt(token)

        case ParseNode   => parseNode(token)
        case ParseScalar => parseScalar(token)

        case ParseMappingEnd         => parseMappingEnd(token)
        case ParseMappingEntry       => parseMappingEntry(token)
        case ParseMappingValue       => parseMappingValue(token)
        case ParseMappingValueNode   => parseMappingValueNode(token)
        case ParseMappingSequenceEnd => parseMappingSequenceEnd(token)
        case ParseMappingEntryOpt    => parseMappingEntryOpt(token)

        case ParseSequenceEnd      => parseSequenceEnd(token)
        case ParseSequenceEntry    => parseSequenceEntry(token)
        case ParseSequenceEntryOpt => parseSequenceEntryOpt(token)

        case ParseFlowNode => parseFlowNode(token)

        case ParseFlowMappingEnd      => parseFlowMappingEnd(token)
        case ParseFlowMappingEntry    => parseFlowMappingEntry(token)
        case ParseFlowMappingEntryOpt => parseFlowMappingEntryOpt(token)
        case ParseFlowMappingComma    => parseFlowMappingComma(token)

        case ParseFlowSeqEnd      => parseFlowSeqEnd(token)
        case ParseFlowSeqEntry    => parseFlowSeqEntry(token)
        case ParseFlowSeqEntryOpt => parseFlowSeqEntryOpt(token)
        case ParseFlowSeqComma    => parseFlowSeqComma(token)

        case ParseFlowSeqPairKey   => parseFlowPairKey(token)
        case ParseFlowSeqPairValue => parseFlowPairValue(token)

        case ReturnEvent(produceEvent) => Right(produceEvent(token))
      }
    }
}

object ParserImpl {
  def apply(in: Tokenizer): ParserImpl = new ParserImpl(in)
}
