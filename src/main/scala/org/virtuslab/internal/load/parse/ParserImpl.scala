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
  case ParseDocumentOptStart
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
 * ParseStreamStart      ::= <ParseDocumentStart> <ParseDocumentOptStart> ParseStreamEnd
 * ParseDocumentStart    ::= <ParseNode> ParseDocumentEnd
 * ParseDocumentOptStart ::= epsilon | <ParseDocumentStart> <ParseDocumentOptStart>
 * ParseNode             ::= <ParseMappingStart> | <ParseSequenceStart> | ParseScalar
 * ParseMappingStart     ::= ParseKey <ParseNode> <ParseOptKey> ParseMappingEnd
 * ParseSequenceStart    ::= <ParseNode> <ParseSequenceEntryOpt> ParseSequenceEnd
 * ParseOptKey           ::= epsilon | ParseKey <ParseNode> <ParseOptKey>
 * ParseSequenceEntryOpt ::= epsilon | <ParseNode> ParseSequenceEntryOpt
*/
object ParserImpl extends Parser:
  import Production.*

  def getEvents(in: Reader): Either[YamlError, List[Event]] =
    @tailrec
    def loop(acc: List[Event], productionStack: List[Production]): List[Event] = {
      val (event, next) = ParserImpl.getNextEvent(in, productionStack)
      if event != Event.StreamEnd then loop(acc :+ event, next)
      else acc :+ event
    }
    Right(loop(Nil, List(ParseStreamStart)))

  def getNextEvent(in: Reader, stack: List[Production]): (Event, List[Production]) = {
    val token = in.peekToken()

    def parseStreamStart() = (
      Event.StreamStart,
      ParseDocumentStart :: ParseDocumentOptStart :: ParseStreamEnd :: stack.tail
    )

    def parseDocumentStart() =
      val event = token match
        case Token.DocumentStart =>
          in.popToken()
          Event.DocumentStart(explicit = true)
        case _ =>
          Event.DocumentStart(explicit = false)
      (event, ParseNode :: ParseDocumentEnd :: stack.tail)

    def parseDocumentOptStart() = token match
      case Token.DocumentStart =>
        getNextEvent(in, ParseDocumentStart :: ParseDocumentOptStart :: stack.tail)
      case _ =>
        getNextEvent(in, stack.tail)

    def parseDocumentEnd() =
      val event = token match
        case Token.DocumentEnd =>
          in.popToken()
          Event.DocumentEnd(true)
        case _ =>
          Event.DocumentEnd(false)
      (event, stack.tail)

    def parseMappingStart() =
      in.popToken()
      (Event.MappingStart, ParseKey :: ParseNode :: ParseOptKey :: ParseMappingEnd :: stack.tail)

    def parseMappingEnd() =
      in.popToken()
      (Event.MappingEnd, stack.tail)

    def parseSequenceStart() =
      in.popToken()
      (Event.SequenceStart, ParseNode :: ParseSequenceEntryOpt :: ParseSequenceEnd :: stack.tail)

    def parseSequenceEnd() =
      in.popToken()
      (Event.SequenceEnd, stack.tail)

    def parseKey() = token match
      case Token.Scalar(value, style) =>
        in.popToken()
        (Event.Scalar(value, style), stack.tail)
      case _ => throw new NotImplementedError("parse key")

    def parseOptKey() = token match
      case Token.Scalar(value, style) =>
        getNextEvent(in, ParseKey :: ParseNode :: ParseOptKey :: stack.tail)
      case _ =>
        getNextEvent(in, stack.tail)

    def parseScalar() = token match
      case Token.Scalar(value, style) =>
        in.popToken()
        (Event.Scalar(value, style), stack.tail)
      case _ => throw new NotImplementedError("parse scalar")

    def parseScalarOpt() = token match
      case Token.Scalar(_, _) => parseScalar()
      case _                  => getNextEvent(in, stack.tail)

    def parseNode() = token match
      case Token.MappingStart  => parseMappingStart()
      case Token.SequenceStart => parseSequenceStart()
      case Token.Scalar(_, _)  => parseScalar()
      case _                   => throw new NotImplementedError("parse node")

    def parseNodeOpt() = token match
      case Token.MappingStart | Token.SequenceStart | Token.Scalar(_, _) => parseNode()
      case _ => (Event.Scalar("", ScalarStyle.Plain), stack.tail)

    def parseSequenceEntryOpt() = token match
      case Token.MappingStart | Token.SequenceStart | Token.Scalar(_, _) =>
        getNextEvent(in, ParseNode :: ParseSequenceEntryOpt :: stack.tail)
      case _ => getNextEvent(in, stack.tail)

    stack.headOption match
      case Some(ParseStreamStart)      => parseStreamStart()
      case Some(ParseStreamEnd)        => (Event.StreamEnd, stack.tail)
      case Some(ParseDocumentStart)    => parseDocumentStart()
      case Some(ParseDocumentEnd)      => parseDocumentEnd()
      case Some(ParseDocumentOptStart) => parseDocumentOptStart()
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
