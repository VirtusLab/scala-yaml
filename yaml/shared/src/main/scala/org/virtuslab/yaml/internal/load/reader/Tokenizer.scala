package org.virtuslab.yaml.internal.load.reader

import org.virtuslab.yaml.internal.load.reader.token.{FinalBreak, ScalarStyle, Token}
import org.virtuslab.yaml.internal.load.reader.token.Token.*
import org.virtuslab.yaml.internal.load.reader.token.FinalBreak.*

import scala.util.Try
import scala.annotation.tailrec

trait Tokenizer:
  def peekToken(): Token
  def popToken(): Token

private[yaml] class Scanner(str: CharSequence) extends Tokenizer {

  private val ctx    = ReaderCtx.init
  private val in     = StringReader(str)
  private var indent = 0

  override def peekToken(): Token = ctx.tokens.headOption match
    case Some(token) => token
    case None        => getToken()

  override def popToken(): Token = ctx.tokens.removeHead()

  private def getToken(): Token =
    ctx.tokens.appendAll(getNextTokens())
    ctx.tokens.head

  @tailrec
  private def getNextTokens(): List[Token] =
    skipUntilNextToken()
    in.peek() match
      case Some('-') if isDocumentStart     => parseDocumentStart()
      case Some('-') if in.isNextWhitespace => parseBlockSequence()
      case Some('.') if isDocumentEnd       => parseDocumentEnd()
      case Some('[')                        => parseFlowSequenceStart()
      case Some(']')                        => parseFlowSequenceEnd()
      case Some('{')                        => parseFlowMappingStart()
      case Some('}')                        => parseFlowMappingEnd()
      case Some(',')                        => { in.skipCharacter(); getNextTokens() }
      case Some(_)                          => fetchValue()
      case None                             => ctx.closeOpenedScopes() :+ Token.StreamEnd

  private def isDocumentStart =
    in.peekN(3) == "---" && in.peek(3).exists(_.isWhitespace)

  private def parseDocumentStart(): List[Token] =
    in.skipN(4)
    ctx.parseDocumentStart()

  private def isDocumentEnd =
    in.peekN(3) == "..." && in.peek(3).exists(_.isWhitespace)

  private def parseDocumentEnd(): List[Token] =
    in.skipN(4)
    ctx.parseDocumentEnd()

  private def parseFlowSequenceStart() =
    in.skipCharacter()
    ctx.appendState(ReaderState.FlowSequence)
    List(FlowSequenceStart)

  private def parseFlowSequenceEnd() =
    in.skipCharacter()
    ctx.closeOpenedSequence()

  private def parseFlowMappingStart() =
    in.skipCharacter()
    ctx.appendState(ReaderState.FlowMapping)
    List(FlowMappingStart)

  private def parseFlowMappingEnd() =
    in.skipCharacter()
    ctx.closeOpenedFlowMapping()

  private def parseBlockSequence() =
    ctx.closeOpenedCollectionSequences(indent)
    if (ctx.shouldParseSequenceEntry(indent)) then
      in.skipCharacter()
      indent += 1
      getNextTokens()
    else
      ctx.appendState(ReaderState.Sequence(indent))
      List(SequenceStart)

  private def parseDoubleQuoteValue(): Token =
    val sb = new StringBuilder

    @tailrec
    def readScalar(): String =
      in.peek() match
        case Some('\\') if in.peekNext() == Some('"') =>
          in.skipN(2)
          sb.append("\"")
          readScalar()
        case Some('"') | None =>
          in.skipCharacter()
          sb.result()
        case Some(char) =>
          sb.append(in.read())
          readScalar()

    in.skipCharacter() // skip double quote
    val scalar = readScalar()
    Scalar(scalar, ScalarStyle.DoubleQuoted)

  /**
   * This header is followed by a non-content line break with an optional comment.
   */
  private def parseBlockHeader(): Unit =
    while (in.peek() == Some(' ')) {
      indent += 1
      in.skipCharacter()
    }

    if in.isNewline then
      in.skipCharacter()
      indent = 0
      parseBlockHeader()

  /**
   * final break interpretation - https://yaml.org/spec/1.2/#b-chomped-last(t)
   */
  private def parseFinalBreak(): FinalBreak =
    in.peek() match
      case Some('-') =>
        in.skipCharacter()
        FinalBreak.Strip
      case Some('+') =>
        in.skipCharacter()
        FinalBreak.Keep
      case _ => FinalBreak.Clip

  private def parseLiteral(): Token =
    val sb = new StringBuilder

    in.skipCharacter() // skip |
    val finalBreakType = parseFinalBreak()

    parseBlockHeader()

    val foldedIndent = indent
    skipUntilNextIndent(foldedIndent)

    @tailrec
    def readLiteral(): String =
      in.peek() match
        case Some('\n') =>
          sb.append(in.read())
          skipUntilNextIndent(foldedIndent)
          if (!in.isWhitespace && indent != foldedIndent) then sb.result()
          else readLiteral()
        case Some(char) =>
          sb.append(in.read())
          readLiteral()
        case None => sb.result()

    val scalar         = readLiteral()
    val choompedScalar = removeBlankLinesAtEnd(scalar, finalBreakType)
    Scalar(choompedScalar, ScalarStyle.Literal)

  private def parseFoldedValue(): Token =
    val sb = new StringBuilder

    in.skipCharacter() // skip >
    val finalBreakType = parseFinalBreak()

    parseBlockHeader()
    val foldedIndent = indent
    skipUntilNextIndent(foldedIndent)

    def chompedEmptyLines() =
      while (in.peekNext() == Some('\n')) {
        in.skipCharacter()
        sb.append("\n")
      }

      in.skipCharacter()
      skipUntilNextIndent(foldedIndent)

    @tailrec
    def readFolded(): String =
      in.peek() match
        case _ if in.isNewline =>
          if (in.peekNext() == Some('\n') && in.peek(2) != None) {
            chompedEmptyLines()
            readFolded()
          } else {
            in.skipCharacter()
            skipUntilNextIndent(foldedIndent)
            if (!in.isWhitespace && indent != foldedIndent) then sb.result()
            else
              sb.append(" ")
              readFolded()
          }
        case Some(char) =>
          sb.append(in.read())
          readFolded()
        case None => sb.result()

    val scalar         = readFolded()
    val choompedScalar = removeBlankLinesAtEnd(scalar, finalBreakType)
    Scalar(choompedScalar, ScalarStyle.Folded)

  private def removeBlankLinesAtEnd(scalar: String, finalBreakType: FinalBreak): String =
    finalBreakType match
      case FinalBreak.Keep  => FinalBreak.choompedKeepType(scalar)
      case FinalBreak.Clip  => FinalBreak.choompedClipType(scalar)
      case FinalBreak.Strip => FinalBreak.choompedStripType(scalar)

  private def parseSingleQuoteValue(): Token = {
    val sb                = new StringBuilder
    val singleQuoteIndent = indent
    @tailrec
    def readScalar(): String =
      in.peek() match
        case Some('\'') if in.peekNext() == Some('\'') =>
          in.skipN(2)
          sb.append('\'')
          readScalar()
        case Some('\n') =>
          in.skipCharacter()
          skipUntilNextIndent(singleQuoteIndent)
          readScalar()
        case Some('\'') | None =>
          in.skipCharacter()
          sb.result()
        case Some(char) =>
          sb.append(in.read())
          readScalar()

    in.skipCharacter() // skip single quote
    val scalar = readScalar()
    Scalar(scalar, ScalarStyle.SingleQuoted)
  }

  private def parseScalarValue(): Token = {
    val sb = new StringBuilder
    def readScalar(): String =
      in.peek() match
        case Some(':')
            if in.peekNext() == Some(' ') || in.peekNext() == Some('\n') || in
              .peekNext() == Some('\r') =>
          sb.result()
        case Some(char) if !ctx.isAllowedSpecialCharacter(char) => sb.result()
        case Some(' ') if in.peekNext() == Some('#')            => sb.result()
        case Some('\n') | Some('\r') | None                     => sb.result()
        case Some(char) =>
          sb.append(in.read())
          readScalar()

    Scalar(readScalar().trim, ScalarStyle.Plain)
  }

  private def fetchValue(): List[Token] =
    skipUntilNextToken()
    val scalar: Token = in.peek() match
      case Some('"')  => parseDoubleQuoteValue()
      case Some('\'') => parseSingleQuoteValue()
      case Some('>')  => parseFoldedValue()
      case Some('|')  => parseLiteral()
      case _          => parseScalarValue()

    in.peek() match
      case Some(':') =>
        ctx.closeOpenedCollectionMapping(indent)
        in.skipCharacter()

        if (ctx.shouldParseMappingEntry(indent)) then List(Token.Key, scalar, Token.Value)
        else if (!ctx.isFlowMapping()) then
          ctx.appendState(ReaderState.Mapping(indent))
          List(MappingStart, Token.Key, scalar, Token.Value)
        else List(scalar)
      case _ => List(scalar)

  def skipUntilNextToken(): Unit =
    while (in.peek() == Some(' ')) do
      indent += 1
      in.skipCharacter()

    if in.peek() == Some('#') then skipComment()

    if (in.isNewline) then {
      in.skipCharacter()
      indent = 0
      skipUntilNextToken()
    }

  def skipUntilNextIndent(indentBlock: Int): Unit =
    indent = 0
    while (in.peek() == Some(' ') && indent < indentBlock) do
      indent += 1
      in.skipCharacter()

  private def skipComment(): Unit = while !in.isNewline do in.skipCharacter()
}
