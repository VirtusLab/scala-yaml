package org.virtuslab.internal.load.parse

import org.virtuslab.internal.YamlError
import org.virtuslab.internal.ParseError
import org.virtuslab.internal.load.reader.Reader
import org.virtuslab.internal.load.reader.token.ScalarStyle
import org.virtuslab.internal.load.reader.token.Token

import scala.annotation.tailrec

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
  case ParseSequenceEnd
  case ParseNode
  case ParseKey
  case ParseValue
  case ParseOptKey
  case ParseScalar
  case ParseFlowMappingEntry
  case ParseSequenceEntryOpt

/**
 * ParserImpl is using following productions:
 * 
 * ParseStreamStart      ::= <ParseDocumentStart> <ParseDocumentStartOpt> ParseStreamEnd
 * ParseDocumentStart    ::= <ParseNode> ParseDocumentEnd
 * ParseDocumentStartOpt ::= epsilon | <ParseDocumentStart> <ParseDocumentStartOpt>
 * ParseNode             ::= <ParseMappingStart> | <ParseFlowMappingStart> | <ParseSequenceStart> | ParseScalar
 * ParseMappingStart     ::= <ParseKey> ParseMappingEnd
 * ParseFlowMappingStart ::= <ParseFlowMappingEntry> ParseFlowMappingEnd
 * ParseFlowMappingEntry ::= <ParseScalar> | <ParseFlowMappingEntry> <ParseFlowMappingEnd> <ParseNode>
 * ParseSequenceStart    ::= <ParseNode> <ParseSequenceEntryOpt> ParseSequenceEnd
 * ParseKey              ::= <ParseScalar> <ParseValue>
 * ParseOptKey           ::= epsilon | <ParseKey>
 * ParseValue            ::= <ParseNode> <ParseOptKey>
 * ParseSequenceEntryOpt ::= epsilon | <ParseNode> ParseSequenceEntryOpt
*/
object ParserImpl extends Parser:
  import Production.*

  override def getEvents(in: Reader): Either[YamlError, List[Event]] =
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

  def getNextEvent(in: Reader, stack: List[Production]): EventResult = {
    val token = in.peekToken()

    def parseStreamStart() = Right(
      Event.StreamStart,
      ParseDocumentStart :: ParseDocumentStartOpt :: ParseStreamEnd :: stack.tail
    )

    def parseDocumentStart() = token match
      case Token.DocumentStart =>
        in.popToken()
        Right(Event.DocumentStart(explicit = true), ParseNode :: ParseDocumentEnd :: stack.tail)
      case _ =>
        Right(Event.DocumentStart(explicit = false), ParseNode :: ParseDocumentEnd :: stack.tail)

    def parseDocumentStartOpt() = token match
      case Token.DocumentStart =>
        getNextEvent(in, ParseDocumentStart :: ParseDocumentStartOpt :: stack.tail)
      case _ =>
        getNextEvent(in, stack.tail)

    def parseDocumentEnd() = token match
      case Token.DocumentEnd =>
        in.popToken()
        Right(Event.DocumentEnd(true), stack.tail)
      case _ =>
        Right(Event.DocumentEnd(false), stack.tail)

    def parseMappingStart() = token match
      case Token.MappingStart =>
        in.popToken()
        Right(Event.MappingStart, ParseKey :: ParseMappingEnd :: stack.tail)
      case other @ _ =>
        Left(ParseError.from(Token.MappingStart, other))

    def parseFlowMappingStart(): EventResult = token match
      case Token.FlowMappingStart =>
        in.popToken()
        Right(Event.FlowMappingStart, ParseFlowMappingEntry :: ParseFlowMappingEnd :: stack.tail)
      case other @ _ =>
        Left(ParseError.from(Token.FlowMappingStart, other))

    def parseFlowMappingEnd(): EventResult = token match
      case Token.FlowMappingEnd =>
        in.popToken()
        Right(Event.FlowMappingEnd, stack.tail)
      case other @ _ =>
        Left(ParseError.from(Token.FlowMappingEnd, other))

    def parseFlowMappingEntry(): EventResult =
      token match
        case Token.Scalar(value, style) =>
          in.popToken()
          Right(Event.Scalar(value, style), ParseNode :: ParseFlowMappingEntry :: stack.tail)
        case Token.FlowMappingStart => {
          in.popToken()
          Right(
            Event.FlowMappingStart,
            ParseFlowMappingEntry :: ParseFlowMappingEnd :: ParseNode :: stack.tail
          )
        }
        case _ =>
          getNextEvent(in, stack.tail)

    def parseMappingEnd() = token match
      case Token.MappingEnd =>
        in.popToken()
        Right(Event.MappingEnd, stack.tail)
      case other @ _ =>
        Left(ParseError.from(Token.MappingEnd, other))

    def parseSequenceStart() = token match
      case Token.SequenceStart =>
        in.popToken()
        Right(
          Event.SequenceStart,
          ParseNode :: ParseSequenceEntryOpt :: ParseSequenceEnd :: stack.tail
        )
      case other @ _ =>
        Left(ParseError.from(Token.SequenceStart, other))

    def parseSequenceEnd() = token match
      case Token.SequenceEnd =>
        in.popToken()
        Right(Event.SequenceEnd, stack.tail)
      case other @ _ =>
        Left(ParseError.from(Token.SequenceEnd, other))

    def parseKey() = token match
      case Token.Key =>
        in.popToken()
        getNextEvent(in, ParseScalar :: ParseValue :: stack.tail)
      case other @ _ =>
        Left(ParseError.from(Token.Key, other))

    def parseOptKey() = token match
      case Token.Key => parseKey()
      case _ =>
        getNextEvent(in, stack.tail)

    def parseValue() = token match
      case Token.Value =>
        in.popToken()
        getNextEvent(in, ParseNode :: ParseOptKey :: stack.tail)
      case other @ _ => Left(ParseError.from(Token.Value, other))

    def parseScalar() = token match
      case Token.Scalar(value, style) =>
        in.popToken()
        Right(Event.Scalar(value, style), stack.tail)
      case other @ _ =>
        Left(ParseError.from("Token.Scalar", other))

    def parseNode(): EventResult = token match
      case Token.MappingStart     => parseMappingStart()
      case Token.FlowMappingStart => parseFlowMappingStart()
      case Token.SequenceStart    => parseSequenceStart()
      case Token.Scalar(_, _)     => parseScalar()
      case _                      => Right(Event.Scalar("", ScalarStyle.Plain), stack.tail)

    def parseSequenceEntryOpt() = token match
      case Token.MappingStart | Token.SequenceStart | Token.Scalar(_, _) =>
        getNextEvent(in, ParseNode :: ParseSequenceEntryOpt :: stack.tail)
      case _ => getNextEvent(in, stack.tail)

    stack.headOption match
      case Some(ParseStreamStart)      => parseStreamStart()
      case Some(ParseStreamEnd)        => Right(Event.StreamEnd, stack.tail)
      case Some(ParseDocumentStart)    => parseDocumentStart()
      case Some(ParseDocumentEnd)      => parseDocumentEnd()
      case Some(ParseDocumentStartOpt) => parseDocumentStartOpt()
      case Some(ParseNode)             => parseNode()
      case Some(ParseSequenceEntryOpt) => parseSequenceEntryOpt()
      case Some(ParseMappingStart)     => parseMappingStart()
      case Some(ParseMappingEnd)       => parseMappingEnd()
      case Some(ParseFlowMappingEnd)   => parseFlowMappingEnd()
      case Some(ParseSequenceStart)    => parseSequenceStart()
      case Some(ParseSequenceEnd)      => parseSequenceEnd()
      case Some(ParseKey)              => parseKey()
      case Some(ParseValue)            => parseValue()
      case Some(ParseOptKey)           => parseOptKey()
      case Some(ParseScalar)           => parseScalar()
      case Some(ParseFlowMappingEntry) => parseFlowMappingEntry()
      case None                        => ???

  }
