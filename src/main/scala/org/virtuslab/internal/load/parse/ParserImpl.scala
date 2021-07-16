package org.virtuslab.internal.load.parse

import org.virtuslab.internal.YamlError
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
  case ParseSequenceStart
  case ParseSequenceEnd
  case ParseNode
  case ParseNodeOpt
  case ParseKey
  case ParseOptKey
  case ParseScalar
  case ParseSequenceEntryOpt

/**
 * ParserImpl is using following productions:
 * 
 * ParseStreamStart      ::= <ParseDocumentStart> <ParseDocumentStartOpt> ParseStreamEnd
 * ParseDocumentStart    ::= <ParseNode> ParseDocumentEnd
 * ParseDocumentStartOpt ::= epsilon | <ParseDocumentStart> <ParseDocumentStartOpt>
 * ParseNode             ::= <ParseMappingStart> | <ParseSequenceStart> | ParseScalar
 * ParseMappingStart     ::= ParseKey <ParseNode> <ParseOptKey> ParseMappingEnd
 * ParseSequenceStart    ::= <ParseNode> <ParseSequenceEntryOpt> ParseSequenceEnd
 * ParseOptKey           ::= epsilon | ParseKey <ParseNode> <ParseOptKey>
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

  private def error(expected: String, got: Token): EventResult = Left(
    YamlError(s"Expected $expected but got $got instead of.")
  )
  private def error(expected: Token, got: Token): EventResult =
    error(expected.toString, got)

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
        Right(
          Event.MappingStart,
          ParseKey :: ParseNode :: ParseOptKey :: ParseMappingEnd :: stack.tail
        )
      case other @ _ =>
        error(Token.MappingStart, other)

    def parseMappingEnd() = token match
      case Token.MappingEnd =>
        in.popToken()
        Right(Event.MappingEnd, stack.tail)
      case other @ _ =>
        error(Token.MappingStart, other)

    def parseSequenceStart() = token match
      case Token.SequenceStart =>
        in.popToken()
        Right(
          Event.SequenceStart,
          ParseNode :: ParseSequenceEntryOpt :: ParseSequenceEnd :: stack.tail
        )
      case other @ _ =>
        error(Token.SequenceStart, other)

    def parseSequenceEnd() = token match
      case Token.SequenceEnd =>
        in.popToken()
        Right(Event.SequenceEnd, stack.tail)
      case other @ _ =>
        error(Token.SequenceEnd, other)

    def parseKey() = token match
      case Token.Scalar(value, style) =>
        in.popToken()
        Right(Event.Scalar(value, style), stack.tail)
      case other @ _ =>
        error("Token.Scalar", other)

    def parseOptKey() = token match
      case Token.Scalar(value, style) =>
        getNextEvent(in, ParseKey :: ParseNode :: ParseOptKey :: stack.tail)
      case _ =>
        getNextEvent(in, stack.tail)

    def parseScalar() = token match
      case Token.Scalar(value, style) =>
        in.popToken()
        Right(Event.Scalar(value, style), stack.tail)
      case other @ _ =>
        error("Token.Scalar", other)

    def parseScalarOpt() = token match
      case Token.Scalar(_, _) => parseScalar()
      case _                  => getNextEvent(in, stack.tail)

    def parseNode() = token match
      case Token.MappingStart  => parseMappingStart()
      case Token.SequenceStart => parseSequenceStart()
      case Token.Scalar(_, _)  => parseScalar()
      case other @ _ =>
        error("Token: MappingStart | SequenceStart | Scalar", other)

    def parseNodeOpt() = token match
      case Token.MappingStart | Token.SequenceStart | Token.Scalar(_, _) => parseNode()
      case _ => Right(Event.Scalar("", ScalarStyle.Plain), stack.tail)

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
      case Some(ParseNodeOpt)          => parseNodeOpt()
      case Some(ParseSequenceEntryOpt) => parseSequenceEntryOpt()
      case Some(ParseMappingStart)     => parseMappingStart()
      case Some(ParseMappingEnd)       => parseMappingEnd()
      case Some(ParseSequenceStart)    => parseSequenceStart()
      case Some(ParseSequenceEnd)      => parseSequenceEnd()
      case Some(ParseKey)              => parseKey()
      case Some(ParseOptKey)           => parseOptKey()
      case Some(ParseScalar)           => parseScalar()
      case None                        => ???

  }
