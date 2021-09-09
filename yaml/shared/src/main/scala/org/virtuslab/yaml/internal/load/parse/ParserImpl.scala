package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.ParseError
import org.virtuslab.yaml.Position
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.reader.Tokenizer
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.Token

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
  case ParseNode
  case ParseKey
  case ParseValue
  case ParseOptKey
  case ParseScalar
  case ParseFlowMappingEntry
  case ParseSequenceEntryOpt

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
 * ParseSequenceStart    ::= <ParseNode> <ParseSequenceEntryOpt> ParseSequenceEnd
 * ParseFlowSequenceStart::= <ParseSequenceEntryOpt> ParseFlowSequenceEnd
 * ParseKey              ::= <ParseScalar> <ParseValue>
 * ParseOptKey           ::= epsilon | <ParseKey>
 * ParseValue            ::= <ParseNode> <ParseOptKey>
 * ParseSequenceEntryOpt ::= epsilon | <ParseNode> ParseSequenceEntryOpt
*/
class ParserImpl(in: Tokenizer) extends Parser:
  import Production.*

  private val productions = mutable.ArrayDeque(ParseStreamStart)

  override def getEvents(): Either[YamlError, List[Event]] =
    @tailrec
    def loop(
        acc: List[Event]
    ): Either[YamlError, List[Event]] = {
      getNextEvent() match
        case Right(event) =>
          if event != Event.StreamEnd then loop(event :: acc)
          else Right((event :: acc).reverse)
        case Left(err) => Left(err)
    }
    loop(Nil)

  override def getNextEvent(): Either[YamlError, Event] =
    if productions.size > 0 then getNextEventImpl()
    else
      Left(
        ParseError("Invalid call of getNextEvent, there is no more events after Event.StreamEnd")
      )

  private def getNextEventImpl(): Either[YamlError, Event] =
    val token = in.peekToken()

    def parseStreamStart() =
      productions.prependAll(ParseDocumentStart :: ParseDocumentStartOpt :: ParseStreamEnd :: Nil)
      Right(Event.StreamStart)

    def parseDocumentStart() = token match
      case Token.DocumentStart(pos: Position) =>
        in.popToken()
        productions.prependAll(ParseNode :: ParseDocumentEnd :: Nil)
        Right(Event.DocumentStart(Some(pos), explicit = true))
      case _ =>
        productions.prependAll(ParseNode :: ParseDocumentEnd :: Nil)
        Right(Event.DocumentStart(Some(token.pos), explicit = false))

    def parseDocumentStartOpt() = token match
      case _: Token.DocumentStart =>
        productions.prependAll(ParseDocumentStart :: ParseDocumentStartOpt :: Nil)
        getNextEvent()
      case _ =>
        getNextEvent()

    def parseDocumentEnd() = token match
      case Token.DocumentEnd(pos) =>
        in.popToken()
        Right(Event.DocumentEnd(Some(pos), true))
      case _ =>
        Right(Event.DocumentEnd(Some(token.pos), false))

    def parseMappingStart() = token match
      case Token.MappingStart(pos) =>
        in.popToken()
        productions.prependAll(ParseKey :: ParseMappingEnd :: Nil)
        Right(Event.MappingStart(Some(pos)))
      case other @ _ =>
        Left(ParseError.from(Token.MappingStart(token.pos), other))

    def parseFlowMappingStart() = token match
      case Token.FlowMappingStart(pos) =>
        in.popToken()
        productions.prependAll(ParseFlowMappingEntry :: ParseFlowMappingEnd :: Nil)
        Right(Event.FlowMappingStart(Some(pos)))
      case other @ _ =>
        Left(ParseError.from(Token.FlowMappingStart(token.pos), other))

    def parseFlowMappingEnd() = token match
      case Token.FlowMappingEnd(pos) =>
        in.popToken()
        Right(Event.FlowMappingEnd(Some(pos)))
      case other @ _ =>
        Left(ParseError.from(Token.FlowMappingEnd(token.pos), other))

    def parseFlowMappingEntry() =
      token match
        case Token.Scalar(value, style, pos) =>
          in.popToken()
          productions.prependAll(ParseNode :: ParseFlowMappingEntry :: Nil)
          Right(Event.Scalar(value, style, Some(pos)))

        case Token.FlowMappingStart(pos) => {
          in.popToken()
          productions.prependAll(ParseFlowMappingEntry :: ParseFlowMappingEnd :: ParseNode :: Nil)

          Right(Event.FlowMappingStart(Some(pos)))
        }
        case _ =>
          getNextEvent()

    def parseMappingEnd() = token match
      case Token.MappingEnd(pos) =>
        in.popToken()
        Right(Event.MappingEnd(Some(pos)))
      case other @ _ =>
        Left(ParseError.from(Token.MappingEnd(token.pos), other))

    def parseSequenceStart() = token match
      case Token.SequenceStart(pos) =>
        in.popToken()
        productions.prependAll(ParseNode :: ParseSequenceEntryOpt :: ParseSequenceEnd :: Nil)

        Right(Event.SequenceStart(Some(pos)))
      case other @ _ =>
        Left(ParseError.from(Token.SequenceStart(token.pos), other))

    def parseSequenceEnd() = token match
      case Token.SequenceEnd(pos) =>
        in.popToken()
        Right(Event.SequenceEnd(Some(pos)))
      case other @ _ =>
        Left(ParseError.from(Token.SequenceEnd(token.pos), other))

    def parseFlowSequenceStart() = token match
      case Token.FlowSequenceStart(pos) =>
        in.popToken()
        productions.prependAll(ParseSequenceEntryOpt :: ParseFlowSequenceEnd :: Nil)

        Right(Event.SequenceStart(Some(pos)))
      case other @ _ =>
        Left(ParseError.from(Token.FlowSequenceStart(token.pos), other))

    def parseFlowSequenceEnd() = token match
      case Token.FlowSequenceEnd(pos) =>
        in.popToken()
        Right(Event.SequenceEnd(Some(pos)))
      case other @ _ =>
        Left(ParseError.from(Token.FlowSequenceEnd(token.pos), other))

    def parseKey() = token match
      case Token.Key(_) =>
        in.popToken()
        productions.prependAll(ParseScalar :: ParseValue :: Nil)
        getNextEvent()
      case other @ _ =>
        Left(ParseError.from(Token.Key(token.pos), other))

    def parseOptKey() = token match
      case Token.Key(_) => parseKey()
      case _ =>
        getNextEvent()

    def parseValue() = token match
      case Token.Value(_) =>
        in.popToken()
        productions.prependAll(ParseNode :: ParseOptKey :: Nil)
        getNextEvent()
      case other @ _ => Left(ParseError.from(Token.Value(token.pos), other))

    def parseScalar() = token match
      case Token.Scalar(value, style, pos) =>
        in.popToken()
        Right(Event.Scalar(value, style, Some(pos)))
      case other @ _ =>
        Left(ParseError.from("Token.Scalar", other))

    def parseNode() = token match
      case _: Token.MappingStart      => parseMappingStart()
      case _: Token.FlowMappingStart  => parseFlowMappingStart()
      case _: Token.SequenceStart     => parseSequenceStart()
      case _: Token.FlowSequenceStart => parseFlowSequenceStart()
      case _: Token.Scalar            => parseScalar()
      case _ =>
        Right(Event.Scalar("", ScalarStyle.Plain, Some(token.pos)))

    def parseSequenceEntryOpt() = token match
      case _: Token.MappingStart | _: Token.SequenceStart | _: Token.FlowMappingStart |
          _: Token.FlowSequenceStart | _: Token.Scalar =>
        productions.prependAll(ParseNode :: ParseSequenceEntryOpt :: Nil)
        getNextEvent()
      case _ =>
        getNextEvent()

    productions.removeHead() match
      case ParseStreamStart       => parseStreamStart()
      case ParseStreamEnd         => Right(Event.StreamEnd)
      case ParseDocumentStart     => parseDocumentStart()
      case ParseDocumentEnd       => parseDocumentEnd()
      case ParseDocumentStartOpt  => parseDocumentStartOpt()
      case ParseNode              => parseNode()
      case ParseSequenceEntryOpt  => parseSequenceEntryOpt()
      case ParseMappingStart      => parseMappingStart()
      case ParseMappingEnd        => parseMappingEnd()
      case ParseFlowMappingEnd    => parseFlowMappingEnd()
      case ParseSequenceStart     => parseSequenceStart()
      case ParseFlowSequenceStart => parseFlowSequenceStart()
      case ParseFlowSequenceEnd   => parseFlowSequenceEnd()
      case ParseSequenceEnd       => parseSequenceEnd()
      case ParseKey               => parseKey()
      case ParseValue             => parseValue()
      case ParseOptKey            => parseOptKey()
      case ParseScalar            => parseScalar()
      case ParseFlowMappingEntry  => parseFlowMappingEntry()
  end getNextEventImpl

end ParserImpl
