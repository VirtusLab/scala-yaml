package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.{ParseError, YamlError}
import org.virtuslab.yaml.internal.load.reader.Tokenizer
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.Token

import scala.annotation.tailrec
import org.virtuslab.yaml.internal.load.reader.Position

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
object ParserImpl extends Parser:
  import Production.*

  override def getEvents(in: Tokenizer): Either[YamlError, List[Event]] =
    @tailrec
    def loop(
        acc: List[Event],
        productionStack: List[Production]
    ): Either[YamlError, List[Event]] = {
      ParserImpl.getNextEvent(in, productionStack) match
        case Right((event, next)) =>
          if event != Event.StreamEnd then loop(acc :+ event, next)
          else Right(acc :+ event)
        case Left(err) => Left(err)
    }
    loop(Nil, List(ParseStreamStart))

  type EventResult = Either[YamlError, (Event, List[Production])]

  def getNextEvent(in: Tokenizer, stack: List[Production]): EventResult = {
    val token = in.peekToken()

    def parseStreamStart() = Right(
      Event.StreamStart(None),
      ParseDocumentStart :: ParseDocumentStartOpt :: ParseStreamEnd :: stack.tail
    )

    def parseDocumentStart() = token match
      case Token.DocumentStart(pos) =>
        in.popToken()
        Right(
          Event.DocumentStart(Some(pos), explicit = true),
          ParseNode :: ParseDocumentEnd :: stack.tail
        )
      case _ =>
        Right(
          Event.DocumentStart(Some(token.pos), explicit = false),
          ParseNode :: ParseDocumentEnd :: stack.tail
        )

    def parseDocumentStartOpt() = token match
      case _: Token.DocumentStart =>
        getNextEvent(in, ParseDocumentStart :: ParseDocumentStartOpt :: stack.tail)
      case _ =>
        getNextEvent(in, stack.tail)

    def parseDocumentEnd() = token match
      case Token.DocumentEnd(pos) =>
        in.popToken()
        Right(Event.DocumentEnd(Some(pos), true), stack.tail)
      case _ =>
        Right(Event.DocumentEnd(Some(token.pos), false), stack.tail)

    def parseMappingStart() = token match
      case Token.MappingStart(pos) =>
        in.popToken()
        Right(Event.MappingStart(Some(pos)), ParseKey :: ParseMappingEnd :: stack.tail)
      case other @ _ =>
        Left(ParseError.from(Token.MappingStart(token.pos), other))

    def parseFlowMappingStart(): EventResult = token match
      case Token.FlowMappingStart(pos) =>
        in.popToken()
        Right(
          Event.FlowMappingStart(Some(pos)),
          ParseFlowMappingEntry :: ParseFlowMappingEnd :: stack.tail
        )
      case other @ _ =>
        Left(ParseError.from(Token.FlowMappingStart(token.pos), other))

    def parseFlowMappingEnd(): EventResult = token match
      case Token.FlowMappingEnd(pos) =>
        in.popToken()
        Right(Event.FlowMappingEnd(Some(pos)), stack.tail)
      case other @ _ =>
        Left(ParseError.from(Token.FlowMappingEnd(token.pos), other))

    def parseFlowMappingEntry(): EventResult =
      token match
        case Token.Scalar(value, style, pos) =>
          in.popToken()
          Right(
            Event.Scalar(value, style, Some(pos)),
            ParseNode :: ParseFlowMappingEntry :: stack.tail
          )
        case Token.FlowMappingStart(pos) => {
          in.popToken()
          Right(
            Event.FlowMappingStart(Some(pos)),
            ParseFlowMappingEntry :: ParseFlowMappingEnd :: ParseNode :: stack.tail
          )
        }
        case _ =>
          getNextEvent(in, stack.tail)

    def parseMappingEnd() = token match
      case Token.MappingEnd(pos) =>
        in.popToken()
        Right(Event.MappingEnd(Some(pos)), stack.tail)
      case other @ _ =>
        Left(ParseError.from(Token.MappingEnd(token.pos), other))

    def parseSequenceStart() = token match
      case Token.SequenceStart(pos) =>
        in.popToken()
        Right(
          Event.SequenceStart(Some(pos)),
          ParseNode :: ParseSequenceEntryOpt :: ParseSequenceEnd :: stack.tail
        )
      case other @ _ =>
        Left(ParseError.from(Token.SequenceStart(token.pos), other))

    def parseSequenceEnd() = token match
      case Token.SequenceEnd(pos) =>
        in.popToken()
        Right(Event.SequenceEnd(Some(pos)), stack.tail)
      case other @ _ =>
        Left(ParseError.from(Token.SequenceEnd(token.pos), other))

    def parseFlowSequenceStart() = token match
      case Token.FlowSequenceStart(pos) =>
        in.popToken()
        Right(
          Event.SequenceStart(Some(pos)),
          ParseSequenceEntryOpt :: ParseFlowSequenceEnd :: stack.tail
        )
      case other @ _ =>
        Left(ParseError.from(Token.FlowSequenceStart(token.pos), other))

    def parseFlowSequenceEnd() = token match
      case Token.FlowSequenceEnd(pos) =>
        in.popToken()
        Right(Event.SequenceEnd(Some(pos)), stack.tail)
      case other @ _ =>
        Left(ParseError.from(Token.FlowSequenceEnd(token.pos), other))

    def parseKey() = token match
      case Token.Key(_) =>
        in.popToken()
        getNextEvent(in, ParseScalar :: ParseValue :: stack.tail)
      case other @ _ =>
        Left(ParseError.from(Token.Key(token.pos), other))

    def parseOptKey() = token match
      case Token.Key(_) => parseKey()
      case _ =>
        getNextEvent(in, stack.tail)

    def parseValue() = token match
      case Token.Value(_) =>
        in.popToken()
        getNextEvent(in, ParseNode :: ParseOptKey :: stack.tail)
      case other @ _ => Left(ParseError.from(Token.Value(token.pos), other))

    def parseScalar() = token match
      case Token.Scalar(value, style, pos) =>
        in.popToken()
        Right(Event.Scalar(value, style, Some(pos)), stack.tail)
      case other @ _ =>
        Left(ParseError.from("Token.Scalar", other))

    def parseNode(): EventResult = token match
      case _: Token.MappingStart      => parseMappingStart()
      case _: Token.FlowMappingStart  => parseFlowMappingStart()
      case _: Token.SequenceStart     => parseSequenceStart()
      case _: Token.FlowSequenceStart => parseFlowSequenceStart()
      case _: Token.Scalar            => parseScalar()
      case _ =>
        Right(Event.Scalar("", ScalarStyle.Plain, Some(token.pos)), stack.tail)

    def parseSequenceEntryOpt() = token match
      case _: Token.MappingStart | _: Token.SequenceStart | _: Token.FlowMappingStart |
          _: Token.FlowSequenceStart | _: Token.Scalar =>
        getNextEvent(in, ParseNode :: ParseSequenceEntryOpt :: stack.tail)
      case _ => getNextEvent(in, stack.tail)

    stack.headOption match
      case Some(ParseStreamStart)       => parseStreamStart()
      case Some(ParseStreamEnd)         => Right(Event.StreamEnd(None), stack.tail)
      case Some(ParseDocumentStart)     => parseDocumentStart()
      case Some(ParseDocumentEnd)       => parseDocumentEnd()
      case Some(ParseDocumentStartOpt)  => parseDocumentStartOpt()
      case Some(ParseNode)              => parseNode()
      case Some(ParseSequenceEntryOpt)  => parseSequenceEntryOpt()
      case Some(ParseMappingStart)      => parseMappingStart()
      case Some(ParseMappingEnd)        => parseMappingEnd()
      case Some(ParseFlowMappingEnd)    => parseFlowMappingEnd()
      case Some(ParseSequenceStart)     => parseSequenceStart()
      case Some(ParseFlowSequenceStart) => parseFlowSequenceStart()
      case Some(ParseFlowSequenceEnd)   => parseFlowSequenceEnd()
      case Some(ParseSequenceEnd)       => parseSequenceEnd()
      case Some(ParseKey)               => parseKey()
      case Some(ParseValue)             => parseValue()
      case Some(ParseOptKey)            => parseOptKey()
      case Some(ParseScalar)            => parseScalar()
      case Some(ParseFlowMappingEntry)  => parseFlowMappingEntry()
      case None                         => ???

  }
