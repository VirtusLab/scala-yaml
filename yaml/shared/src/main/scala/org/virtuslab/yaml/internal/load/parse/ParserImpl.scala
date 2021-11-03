package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.ParseError
import org.virtuslab.yaml.Position
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.reader.Tokenizer
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind

import scala.annotation.tailrec
import scala.collection.mutable

private enum Production:
  case ParseStreamStart
  case ParseStreamEnd
  case ParseDocumentStart
  case ParseDocumentEnd
  case ParseDocumentStartOpt

  case ParseNode
  case ParseScalar

  case ParseMappingEnd
  case ParseMappingEntry
  case ParseMappingValue
  case ParseMappingValueNode
  case ParseMappingSequenceEnd
  case ParseMappingEntryOpt

  case ParseSequenceEnd
  case ParseSequenceEntry
  case ParseSequenceEntryOpt

  case ParseFlowNode

  case ParseFlowMappingEnd
  case ParseFlowMappingEntry
  case ParseFlowMappingEntryOpt
  case ParseFlowMappingComma

  case ParseFlowSeqEnd
  case ParseFlowSeqEntry
  case ParseFlowSeqEntryOpt
  case ParseFlowSeqPairKey
  case ParseFlowSeqPairValue
  case ParseFlowSeqComma

  case ReturnEvent(produceEvent: Token => Event)

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
final class ParserImpl private (in: Tokenizer) extends Parser:
  import Production.*

  private val productions = mutable.ArrayDeque(ParseStreamStart)

  private[yaml] def getEvents(): Either[YamlError, List[Event]] =
    @tailrec
    def loop(events: mutable.ArrayDeque[Event]): Either[YamlError, List[Event]] = {
      getNextEvent() match
        case Right(event) =>
          println(event)
          if event != Event.StreamEnd then loop(events.append(event))
          else Right(events.append(event).toList)
        case Left(err) => Left(err)
    }
    loop(new mutable.ArrayDeque())

  override def getNextEvent(): Either[YamlError, Event] =
    if productions.size > 0 then getNextEventImpl()
    else Right(Event.StreamEnd)

  private def getNextEventImpl(): Either[YamlError, Event] =
    val token = in.peekToken()
    val pos   = token.pos

    def parseStreamStart() =
      productions.prependAll(ParseDocumentStartOpt :: ParseStreamEnd :: Nil)
      Right(Event.StreamStart)

    def parseDocumentStart() = token.kind match
      case TokenKind.DocumentStart =>
        in.popToken()
        productions.prependAll(ParseNode :: ParseDocumentEnd :: Nil)
        Right(Event.DocumentStart(Some(pos), explicit = true))
      case _ =>
        productions.prependAll(ParseNode :: ParseDocumentEnd :: Nil)
        Right(Event.DocumentStart(Some(token.pos), explicit = false))

    def parseDocumentStartOpt() = token.kind match
      case TokenKind.DocumentStart =>
        productions.prependAll(ParseDocumentStart :: ParseDocumentStartOpt :: Nil)
        getNextEventImpl()
      case TokenKind.MappingStart | TokenKind.Scalar(_, _) | TokenKind.SequenceStart |
          TokenKind.FlowMappingStart | TokenKind.FlowSequenceStart | _: TokenKind.Anchor =>
        productions.prependAll(ParseDocumentStart :: ParseDocumentStartOpt :: Nil)
        getNextEventImpl()
      case _ =>
        getNextEventImpl()

    def parseDocumentEnd() = token.kind match
      case TokenKind.DocumentEnd =>
        in.popToken()
        Right(Event.DocumentEnd(Some(pos), true))
      case _ =>
        Right(Event.DocumentEnd(Some(token.pos), false))

    def parseMappingEnd() = token.kind match
      case TokenKind.BlockEnd =>
        in.popToken()
        Right(Event.MappingEnd(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.BlockEnd, token))

    def parseMappingEntry() = token.kind match
      case TokenKind.MappingKey =>
        in.popToken()
        productions.prependAll(ParseScalar :: ParseMappingValue :: ParseMappingEntryOpt :: Nil)
        getNextEventImpl()
      case _ =>
        Left(ParseError.from(TokenKind.MappingKey, token))

    def parseMappingValue() = token.kind match
      case TokenKind.MappingValue =>
        in.popToken()
        productions.prependAll(ParseMappingValueNode :: Nil)
        getNextEventImpl()
      case _ => Left(ParseError.from(TokenKind.MappingValue, token))

    def parseMappingValueNode() = token.kind match
      case TokenKind.SequenceValue =>
        productions.prependAll(ParseSequenceEntry :: ParseMappingSequenceEnd :: Nil)
        Right(Event.SequenceStart(Some(pos)))
      case _ => parseNode()

    def parseMappingSequenceEnd() =
      Right(Event.SequenceEnd(Some(pos)))

    def parseMappingEntryOpt() = token.kind match
      case TokenKind.MappingKey =>
        productions.prependAll(ParseMappingEntry :: Nil)
        getNextEventImpl()
      case _ =>
        getNextEventImpl()

    def parseSequenceStart() = token.kind match
      case TokenKind.SequenceStart =>
        in.popToken()
        productions.prependAll(ParseSequenceEntry :: ParseSequenceEnd :: Nil)
        Right(Event.SequenceStart(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.SequenceStart, token))

    def parseSequenceEnd() = token.kind match
      case TokenKind.BlockEnd =>
        in.popToken()
        Right(Event.SequenceEnd(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.BlockEnd, token))

    def parseSequenceEntry() = token.kind match
      case TokenKind.SequenceValue =>
        in.popToken()
        productions.prependAll(ParseNode :: ParseSequenceEntryOpt :: Nil)
        getNextEventImpl()
      case _ => Left(ParseError.from(TokenKind.SequenceValue, token))

    def parseSequenceEntryOpt() = token.kind match
      case TokenKind.SequenceValue =>
        productions.prependAll(ParseSequenceEntry :: Nil)
        getNextEventImpl()
      case _ =>
        getNextEventImpl()

    def parseFlowMappingStart() = token.kind match
      case TokenKind.FlowMappingStart =>
        in.popToken()
        productions.prependAll(ParseFlowMappingEntryOpt :: ParseFlowMappingEnd :: Nil)
        Right(Event.FlowMappingStart(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.FlowMappingStart, token))

    def parseFlowMappingEnd() = token.kind match
      case TokenKind.FlowMappingEnd =>
        in.popToken()
        Right(Event.FlowMappingEnd(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.FlowMappingEnd, token))

    def parseFlowMappingEntry() = token.kind match
      case TokenKind.MappingKey =>
        in.popToken()
        productions.prependAll(ParseScalar :: ParseMappingValue :: ParseFlowMappingComma :: Nil)
        getNextEventImpl()
      case _ =>
        getNextEventImpl()

    def parseFlowMappingEntryOpt() = token.kind match
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
      case TokenKind.Scalar(_, _) =>
        productions.prependAll(
          ParseFlowNode :: ParseFlowMappingComma :: ParseFlowMappingEntry :: Nil
        )
        parseFlowNode()
      case _ =>
        getNextEventImpl()

    def parseFlowMappingValue() = token.kind match
      case TokenKind.MappingValue =>
        in.popToken()
        productions.prependAll(ParseFlowNode :: Nil)
        getNextEventImpl()
      case _ => Left(ParseError.from(TokenKind.MappingValue, token))

    def parseFlowMappingComma() = token.kind match
      case TokenKind.Comma =>
        in.popToken()
        productions.prependAll(ParseFlowMappingEntryOpt :: Nil)
        getNextEventImpl()
      case _ =>
        getNextEventImpl()

    def parseFlowSeqEnd() = token.kind match
      case TokenKind.FlowSequenceEnd =>
        in.popToken()
        Right(Event.SequenceEnd(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.FlowSequenceEnd, token))

    def parseFlowSeqEntry() = token.kind match
      case TokenKind.MappingKey =>
        productions.prependAll(ParseFlowSeqPairKey :: ParseFlowSeqComma :: Nil)
        Right(Event.MappingStart(Some(token.pos)))
      case _ =>
        productions.prependAll(ParseFlowNode :: ParseFlowSeqComma :: Nil)
        getNextEventImpl()

    def parseFlowSeqEntryOpt() = token.kind match
      case TokenKind.FlowMappingStart | TokenKind.FlowSequenceStart | _: TokenKind.Scalar |
          TokenKind.MappingKey =>
        productions.prependAll(ParseFlowSeqEntry :: Nil)
        getNextEventImpl()
      case _ =>
        getNextEventImpl()

    def parseFlowPairKey() = token.kind match
      case TokenKind.MappingKey =>
        in.popToken()
        productions.prependAll(
          ParseFlowNode :: ParseFlowSeqPairValue :: ReturnEvent(t =>
            Event.MappingEnd(Some(t.pos))
          ) :: Nil
        )
        getNextEventImpl()
      case _ =>
        Left(ParseError.from(TokenKind.MappingKey, token))

    def parseFlowPairValue() = token.kind match
      case TokenKind.MappingValue =>
        in.popToken()
        productions.prependAll(ParseFlowNode :: Nil)
        getNextEventImpl()
      case _ =>
        Left(ParseError.from(TokenKind.MappingValue, token))

    def parseFlowSeqComma() = token.kind match
      case TokenKind.Comma =>
        in.popToken()
        productions.prependAll(ParseFlowSeqEntryOpt :: Nil)
        getNextEventImpl()
      case _ =>
        getNextEventImpl()

    def parseScalar() =
      val (anchor, nextToken) = parseNodeAttributes()
      nextToken.kind match
        case TokenKind.Scalar(value, style) =>
          in.popToken()
          Right(Event.Scalar(value, style, Some(pos), anchor))
        case _ =>
          Left(ParseError.from(TokenKind.Scalar.toString, token))

    inline def parseFlowNode() = parseNode(couldParseBlockColl = false)

    def parseNode(couldParseBlockColl: Boolean = true): Either[YamlError, Event] =
      val (anchor, nextToken) = parseNodeAttributes()

      nextToken.kind match
        case TokenKind.Alias(alias) =>
          if anchor.isDefined then Left(ParseError.from("Alias cannot have an anchor", nextToken))
          else
            in.popToken()
            Right(Event.Alias(alias))
        case TokenKind.MappingStart if couldParseBlockColl =>
          in.popToken()
          productions.prependAll(ParseMappingEntry :: ParseMappingEnd :: Nil)
          Right(Event.MappingStart(Some(pos), anchor))
        case TokenKind.SequenceStart if couldParseBlockColl =>
          parseSequenceStart()
        case TokenKind.FlowMappingStart =>
          in.popToken()
          productions.prependAll(ParseFlowMappingEntryOpt :: ParseFlowMappingEnd :: Nil)
          Right(Event.FlowMappingStart(Some(pos)))
        case TokenKind.FlowSequenceStart =>
          in.popToken()
          productions.prependAll(ParseFlowSeqEntryOpt :: ParseFlowSeqEnd :: Nil)
          Right(Event.SequenceStart(Some(pos)))
        case TokenKind.Scalar(value, style) =>
          in.popToken()
          Right(Event.Scalar(value, style, Some(pos), anchor))
        case _ =>
          Right(Event.Scalar("", ScalarStyle.Plain, Some(token.pos), anchor))

    def parseNodeAttributes() = token.kind match
      case TokenKind.Anchor(value) =>
        in.popToken()
        (Some(value), in.peekToken())
      case _ => (None, token)

    productions.removeHead() match
      case ParseStreamStart      => parseStreamStart()
      case ParseStreamEnd        => Right(Event.StreamEnd)
      case ParseDocumentStart    => parseDocumentStart()
      case ParseDocumentEnd      => parseDocumentEnd()
      case ParseDocumentStartOpt => parseDocumentStartOpt()

      case ParseNode   => parseNode()
      case ParseScalar => parseScalar()

      case ParseMappingEnd         => parseMappingEnd()
      case ParseMappingEntry       => parseMappingEntry()
      case ParseMappingValue       => parseMappingValue()
      case ParseMappingValueNode   => parseMappingValueNode()
      case ParseMappingSequenceEnd => parseMappingSequenceEnd()
      case ParseMappingEntryOpt    => parseMappingEntryOpt()

      case ParseSequenceEnd      => parseSequenceEnd()
      case ParseSequenceEntry    => parseSequenceEntry()
      case ParseSequenceEntryOpt => parseSequenceEntryOpt()

      case ParseFlowNode => parseFlowNode()

      case ParseFlowMappingEnd      => parseFlowMappingEnd()
      case ParseFlowMappingEntry    => parseFlowMappingEntry()
      case ParseFlowMappingEntryOpt => parseFlowMappingEntryOpt()
      case ParseFlowMappingComma    => parseFlowMappingComma()

      case ParseFlowSeqEnd      => parseFlowSeqEnd()
      case ParseFlowSeqEntry    => parseFlowSeqEntry()
      case ParseFlowSeqEntryOpt => parseFlowSeqEntryOpt()
      case ParseFlowSeqComma    => parseFlowSeqComma()

      case ParseFlowSeqPairKey   => parseFlowPairKey()
      case ParseFlowSeqPairValue => parseFlowPairValue()

      case ReturnEvent(produceEvent) => Right(produceEvent(token))
  end getNextEventImpl
end ParserImpl

object ParserImpl:
  def apply(in: Tokenizer): ParserImpl = new ParserImpl(in)
