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
  case ParseDocumentStartOpt
  case ParseDocumentEnd
  case ParseMappingStart
  case ParseMappingEnd
  case ParseFlowMappingEnd
  case ParseSequenceStart
  case ParseFlowSequenceStart
  case ParseFlowSequenceEnd
  case ParseSequenceEnd
  case ParseNode(implicitlySeq: Boolean = false)
  case ParseKey
  case ParseValue
  case ParseOptKey
  case ParseScalar
  case ParseFlowMappingEntry
  case ParseSequenceValue
  case ParseSequenceValueOpt
  case ParseImplicitSequenceEnd

/**
 * Parser takes a stream of [[Token]]s and produces a series of serialization [[Event]]s. Parsing can fail due to ill-formed input.
 * 
 * ParserImpl is using following productions:
 * 
 * ParseStreamStart      ::= <ParseDocumentStart> <ParseDocumentStartOpt> ParseStreamEnd
 * ParseDocumentStart    ::= <ParseNode> ParseDocumentEnd
 * ParseDocumentStartOpt ::= epsilon | <ParseDocumentStart> <ParseDocumentStartOpt>
 * ParseNode             ::= <ParseMappingStart> | <ParseFlowMappingStart> | <ParseSequenceStart> | <ParseFlowSequenceStart> | ParseScalar
 * ParseNodeOpt          ::= epsilon | <ParseMappingStart> | <ParseFlowMappingStart> | <ParseSequenceStart> | ParseScalar
 * ParseMappingStart     ::= <ParseKey> ParseMappingEnd
 * ParseFlowMappingStart ::= <ParseFlowMappingEntry> ParseFlowMappingEnd
 * ParseFlowMappingEntry ::= <ParseScalar> | <ParseFlowMappingEntry> <ParseFlowMappingEnd> <ParseNode>
 * ParseSequenceStart    ::= <ParseNode> <ParseSequenceValueOpt> ParseSequenceEnd
 * ParseFlowSequenceStart::= <ParseSequenceValueOpt> ParseFlowSequenceEnd
 * ParseKey              ::= <ParseScalar> <ParseValue>
 * ParseOptKey           ::= epsilon | <ParseKey>
 * ParseValue            ::= <ParseNode> <ParseOptKey>
 * ParseSequenceValue    ::= <ParseNode> ParseSequenceValueOpt
 * ParseSequenceValueOpt ::= epsilon | <ParseNode> ParseSequenceValueOpt
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
        productions.prependAll(ParseNode() :: ParseDocumentEnd :: Nil)
        Right(Event.DocumentStart(Some(pos), explicit = true))
      case _ =>
        productions.prependAll(ParseNode() :: ParseDocumentEnd :: Nil)
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
        productions.prependAll(ParseKey :: ParseMappingEnd :: Nil)
        Right(Event.MappingStart(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.MappingStart, token))

    def parseFlowMappingStart() = token.kind match
      case TokenKind.FlowMappingStart =>
        in.popToken()
        productions.prependAll(ParseFlowMappingEntry :: ParseFlowMappingEnd :: Nil)
        Right(Event.FlowMappingStart(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.FlowMappingStart, token))

    def parseFlowMappingEnd() = token.kind match
      case TokenKind.FlowMappingEnd =>
        in.popToken()
        Right(Event.FlowMappingEnd(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.FlowMappingEnd, token))

    def parseFlowMappingEntry() =
      token.kind match
        case TokenKind.Scalar(value, style) =>
          in.popToken()
          productions.prependAll(ParseNode() :: ParseFlowMappingEntry :: Nil)
          Right(Event.Scalar(value, style, Some(pos)))

        case TokenKind.FlowMappingStart => {
          in.popToken()
          productions.prependAll(ParseFlowMappingEntry :: ParseFlowMappingEnd :: ParseNode() :: Nil)

          Right(Event.FlowMappingStart(Some(pos)))
        }
        case _ =>
          getNextEvent()

    def parseMappingEnd() = token.kind match
      case TokenKind.BlockEnd =>
        in.popToken()
        Right(Event.MappingEnd(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.BlockEnd, token))

    def parseParseImplicitSequenceEnd() =
      Right(Event.SequenceEnd(Some(token.pos)))

    def parseSequenceStart() = token.kind match
      case TokenKind.SequenceStart =>
        in.popToken()
        productions.prependAll(ParseSequenceValue :: ParseSequenceEnd :: Nil)

        Right(Event.SequenceStart(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.SequenceStart, token))

    def parseSequenceEnd() = token.kind match
      case TokenKind.BlockEnd =>
        in.popToken()
        Right(Event.SequenceEnd(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.BlockEnd, token))

    def parseFlowSequenceStart() = token.kind match
      case TokenKind.FlowSequenceStart =>
        in.popToken()
        productions.prependAll(ParseSequenceValueOpt :: ParseFlowSequenceEnd :: Nil)

        Right(Event.SequenceStart(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.FlowSequenceStart, token))

    def parseFlowSequenceEnd() = token.kind match
      case TokenKind.FlowSequenceEnd =>
        in.popToken()
        Right(Event.SequenceEnd(Some(pos)))
      case _ =>
        Left(ParseError.from(TokenKind.FlowSequenceEnd, token))

    def parseKey() = token.kind match
      case TokenKind.MappingKey =>
        in.popToken()
        productions.prependAll(ParseScalar :: ParseValue :: Nil)
        getNextEvent()
      case _ =>
        Left(ParseError.from(TokenKind.MappingKey, token))

    def parseOptKey() = token.kind match
      case TokenKind.MappingKey => parseKey()
      case _ =>
        getNextEvent()

    def parseValue() = token.kind match
      case TokenKind.MappingValue =>
        in.popToken()
        productions.prependAll(ParseNode(implicitlySeq = true) :: ParseOptKey :: Nil)
        getNextEvent()
      case _ => Left(ParseError.from(TokenKind.MappingValue, token))

    def parseScalar() = token.kind match
      case TokenKind.Scalar(value, style) =>
        in.popToken()
        Right(Event.Scalar(value, style, Some(pos)))
      case _ =>
        Left(ParseError.from("TokenKind.Scalar", token))

    def parseNode(implicitlySeq: Boolean): Either[YamlError, Event] = token.kind match
      case TokenKind.MappingStart      => parseMappingStart()
      case TokenKind.FlowMappingStart  => parseFlowMappingStart()
      case TokenKind.SequenceStart     => parseSequenceStart()
      case TokenKind.FlowSequenceStart => parseFlowSequenceStart()
      case TokenKind.Scalar(_, _)      => parseScalar()
      case TokenKind.SequenceValue if implicitlySeq =>
        productions.prependAll(ParseSequenceValue :: ParseImplicitSequenceEnd :: Nil)
        Right(Event.SequenceStart(Some(pos)))
      case _ =>
        Right(Event.Scalar("", ScalarStyle.Plain, Some(token.pos)))

    def parseSequenceValue() = token.kind match
      case TokenKind.SequenceValue =>
        in.popToken()
        productions.prependAll(ParseNode() :: ParseSequenceValueOpt :: Nil)
        getNextEvent()
      case _ => Left(ParseError.from(TokenKind.SequenceValue, token))

    def parseSequenceValueOpt() = token.kind match
      case TokenKind.SequenceValue => parseSequenceValue()
      case TokenKind.MappingStart | TokenKind.SequenceStart | TokenKind.FlowMappingStart |
          TokenKind.FlowSequenceStart | TokenKind.Scalar(_, _) =>
        productions.prependAll(ParseNode() :: ParseSequenceValueOpt :: Nil)
        getNextEvent()
      case _ =>
        getNextEvent()

    productions.removeHead() match
      case ParseStreamStart         => parseStreamStart()
      case ParseStreamEnd           => Right(Event.StreamEnd)
      case ParseDocumentStart       => parseDocumentStart()
      case ParseDocumentEnd         => parseDocumentEnd()
      case ParseDocumentStartOpt    => parseDocumentStartOpt()
      case ParseNode(implicitly)    => parseNode(implicitly)
      case ParseMappingStart        => parseMappingStart()
      case ParseMappingEnd          => parseMappingEnd()
      case ParseFlowMappingEnd      => parseFlowMappingEnd()
      case ParseSequenceStart       => parseSequenceStart()
      case ParseImplicitSequenceEnd => parseParseImplicitSequenceEnd()
      case ParseSequenceValue       => parseSequenceValue()
      case ParseSequenceValueOpt    => parseSequenceValueOpt()
      case ParseFlowSequenceStart   => parseFlowSequenceStart()
      case ParseFlowSequenceEnd     => parseFlowSequenceEnd()
      case ParseSequenceEnd         => parseSequenceEnd()
      case ParseKey                 => parseKey()
      case ParseValue               => parseValue()
      case ParseOptKey              => parseOptKey()
      case ParseScalar              => parseScalar()
      case ParseFlowMappingEntry    => parseFlowMappingEntry()
  end getNextEventImpl

end ParserImpl

object ParserImpl:
  def apply(in: Tokenizer): ParserImpl = new ParserImpl(in)
