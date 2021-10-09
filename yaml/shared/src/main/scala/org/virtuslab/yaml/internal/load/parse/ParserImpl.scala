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

  case ParseNode(indentLess: Boolean)
  case ParseScalar

  case ParseMappingStart
  case ParseMappingEnd
  case ParseMappingEntry
  case ParseMappingValue
  case ParseMappingEntryOpt

  case ParseSequenceStart(indentLess: Boolean)
  case ParseSequenceEnd(indentLess: Boolean)
  case ParseSequenceEntry
  case ParseSequenceEntryOpt

  case ParseFlowMappingStart
  case ParseFlowMappingEnd

  case ParseFlowSequenceStart
  case ParseFlowSequenceEnd
  case ParseFlowSequenceEntry
  case ParseFlowSequenceEntryOpt

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
 *                                   ParseSequenceStart(indentLess) | ParseFlowSequenceStart | ParseScalar
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
 * 
 * ParseFlowMappingStart         ::= flow_mapping_start ParseMappingEntry ParseFlowMappingEnd
 * ParseFlowMappingEnd           ::= flow_mapping_end
 * 
 * 
 * ParseFlowSequenceStart        ::= flow_seq_start ParseFlowSequenceEntryOpt ParseFlowSequenceEnd
 * ParseFlowSequenceEnd          ::= flow_seq_end
 * ParseFlowSequenceEntry        ::= ParseNode ParseFlowSequenceEntryOpt
 * ParseFlowSequenceEntryOpt     ::= epsilon | ParseFlowSequenceEntry
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
      productions.prependAll(ParseDocumentStart :: ParseDocumentStartOpt :: ParseStreamEnd :: Nil)
      Right(Event.StreamStart)

    def parseDocumentStart() = token.kind match
      case TokenKind.DocumentStart =>
        in.popToken()
        productions.prependAll(ParseNode(true) :: ParseDocumentEnd :: Nil)
        Right(Event.DocumentStart(Some(pos), explicit = true))
      case _ =>
        productions.prependAll(ParseNode(true) :: ParseDocumentEnd :: Nil)
        Right(Event.DocumentStart(Some(token.pos), explicit = false))

    def parseDocumentStartOpt() = token.kind match
      case TokenKind.DocumentStart =>
        productions.prependAll(ParseDocumentStart :: ParseDocumentStartOpt :: Nil)
        getNextEvent()
      case _ =>
        getNextEvent()

    def parseDocumentEnd() = token.kind match
      case TokenKind.DocumentEnd =>
        in.popToken()
        Right(Event.DocumentEnd(Some(pos), true))
      case _ =>
        Right(Event.DocumentEnd(Some(token.pos), false))

    def parseMappingStart() = token.kind match
      case TokenKind.MappingStart =>
        in.popToken()
        productions.prependAll(ParseMappingEntry :: ParseMappingEnd :: Nil)
        Right(Event.MappingStart(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.MappingStart, token))

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
        getNextEvent()
      case _ =>
        Left(ParseError.from(TokenKind.MappingKey, token))

    def parseMappingValue() = token.kind match
      case TokenKind.MappingValue =>
        in.popToken()
        productions.prependAll(ParseNode(true) :: Nil)
        getNextEvent()
      case _ => Left(ParseError.from(TokenKind.MappingValue, token))

    def parseMappingEntryOpt() = token.kind match
      case TokenKind.MappingKey =>
        productions.prependAll(ParseMappingEntry :: Nil)
        getNextEvent()
      case _ =>
        val x = productions.toList.toArray
        getNextEvent()

    def parseSequenceStart(indentLess: Boolean) = token.kind match
      case TokenKind.SequenceStart =>
        in.popToken()
        productions.prependAll(ParseSequenceEntry :: ParseSequenceEnd(indentLess) :: Nil)
        Right(Event.SequenceStart(Some(pos)))
      case TokenKind.SequenceValue if indentLess =>
        productions.prependAll(ParseSequenceEntry :: ParseSequenceEnd(indentLess) :: Nil)
        Right(Event.SequenceStart(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.SequenceStart, token))

    def parseSequenceEnd(indentLess: Boolean) = token.kind match
      case TokenKind.BlockEnd if !indentLess =>
        in.popToken()
        Right(Event.SequenceEnd(Some(pos)))
      case _ if indentLess =>
        Right(Event.SequenceEnd(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.BlockEnd, token))

    def parseSequenceEntry() = token.kind match
      case TokenKind.SequenceValue =>
        in.popToken()
        productions.prependAll(ParseNode(false) :: ParseSequenceEntryOpt :: Nil)
        getNextEvent()
      case _ => Left(ParseError.from(TokenKind.SequenceValue, token))

    def parseSequenceEntryOpt() = token.kind match
      case TokenKind.SequenceValue =>
        productions.prependAll(ParseSequenceEntry :: Nil)
        getNextEvent()
      case _ =>
        getNextEvent()

    def parseFlowMappingStart() = token.kind match
      case TokenKind.FlowMappingStart =>
        in.popToken()
        productions.prependAll(ParseMappingEntry :: ParseFlowMappingEnd :: Nil)
        Right(Event.FlowMappingStart(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.FlowMappingStart, token))

    def parseFlowMappingEnd() = token.kind match
      case TokenKind.FlowMappingEnd =>
        in.popToken()
        Right(Event.FlowMappingEnd(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.FlowMappingEnd, token))

    def parseFlowSequenceStart() = token.kind match
      case TokenKind.FlowSequenceStart =>
        in.popToken()
        productions.prependAll(ParseFlowSequenceEntryOpt :: ParseFlowSequenceEnd :: Nil)
        Right(Event.SequenceStart(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.FlowSequenceStart, token))

    def parseFlowSequenceEnd() = token.kind match
      case TokenKind.FlowSequenceEnd =>
        in.popToken()
        Right(Event.SequenceEnd(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.FlowSequenceEnd, token))

    def parseFlowSequenceEntry() =
      productions.prependAll(ParseNode(false) :: ParseFlowSequenceEntryOpt :: Nil)
      getNextEvent()

    def parseFlowSequenceEntryOpt() = token.kind match
      case TokenKind.MappingStart | TokenKind.SequenceStart | TokenKind.FlowMappingStart |
          TokenKind.FlowSequenceStart | _: TokenKind.Scalar =>
        productions.prependAll(ParseFlowSequenceEntry :: Nil)
        getNextEvent()
      case _ =>
        getNextEvent()

    def parseScalar() = token.kind match
      case TokenKind.Scalar(value, style) =>
        in.popToken()
        Right(Event.Scalar(value, style, Some(pos)))
      case _ =>
        Left(ParseError.from("TokenKind.Scalar", token))

    def parseNode(indentLess: Boolean): Either[YamlError, Event] = token.kind match
      case TokenKind.MappingStart      => parseMappingStart()
      case TokenKind.FlowMappingStart  => parseFlowMappingStart()
      case TokenKind.FlowSequenceStart => parseFlowSequenceStart()
      case TokenKind.Scalar(_, _)      => parseScalar()
      case TokenKind.SequenceStart     => parseSequenceStart(indentLess)
      case TokenKind.SequenceValue if indentLess =>
        productions.prependAll(ParseSequenceStart(true) :: Nil)
        getNextEvent()
      case _ =>
        Right(Event.Scalar("", ScalarStyle.Plain, Some(token.pos)))

    productions.removeHead() match
      case ParseStreamStart      => parseStreamStart()
      case ParseStreamEnd        => Right(Event.StreamEnd)
      case ParseDocumentStart    => parseDocumentStart()
      case ParseDocumentEnd      => parseDocumentEnd()
      case ParseDocumentStartOpt => parseDocumentStartOpt()

      case ParseNode(indentLess) => parseNode(indentLess)
      case ParseScalar           => parseScalar()

      case ParseMappingStart    => parseMappingStart()
      case ParseMappingEnd      => parseMappingEnd()
      case ParseMappingEntry    => parseMappingEntry()
      case ParseMappingValue    => parseMappingValue()
      case ParseMappingEntryOpt => parseMappingEntryOpt()

      case ParseSequenceStart(indentLess) => parseSequenceStart(indentLess)
      case ParseSequenceEnd(indentLess)   => parseSequenceEnd(indentLess)
      case ParseSequenceEntry             => parseSequenceEntry()
      case ParseSequenceEntryOpt          => parseSequenceEntryOpt()

      case ParseFlowMappingStart => parseFlowMappingStart()
      case ParseFlowMappingEnd   => parseMappingEnd()

      case ParseFlowSequenceStart    => parseFlowSequenceStart()
      case ParseFlowSequenceEnd      => parseFlowSequenceEnd()
      case ParseFlowSequenceEntry    => parseFlowSequenceEntry()
      case ParseFlowSequenceEntryOpt => parseFlowSequenceEntryOpt()
  end getNextEventImpl
end ParserImpl

object ParserImpl:
  def apply(in: Tokenizer): ParserImpl = new ParserImpl(in)
