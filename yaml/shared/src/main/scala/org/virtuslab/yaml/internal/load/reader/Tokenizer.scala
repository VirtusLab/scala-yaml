package org.virtuslab.yaml.internal.load.reader

import org.virtuslab.yaml.internal.load.reader.token.{BlockChompingIndicator, ScalarStyle, Token}
import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind.*
import org.virtuslab.yaml.internal.load.reader.token.BlockChompingIndicator.*

import scala.util.Try
import scala.annotation.tailrec

trait Tokenizer:
  def peekToken(): Token
  def popToken(): Token

private[yaml] class Scanner(str: String) extends Tokenizer {

  private val ctx = ReaderCtx(str)
  private val in  = ctx.reader

  override def peekToken(): Token = ctx.tokens.headOption match
    case Some(token) => token
    case None        => getToken()

  override def popToken(): Token = ctx.tokens.removeHead()

  private def getToken(): Token =
    ctx.tokens.append(getNextTokens())
    ctx.tokens.head

  @tailrec
  private def getNextTokens(): Token =
    skipUntilNextToken()
    ctx.checkIndents(in.column)
    val peeked = in.peek()
    peeked match
      case Some('-') if isDocumentStart     => parseDocumentStart()
      case Some('-') if in.isNextWhitespace => parseBlockSequence()
      case Some('.') if isDocumentEnd       => parseDocumentEnd()
      case Some('[')                        => parseFlowSequenceStart()
      case Some(']')                        => parseFlowSequenceEnd()
      case Some('{')                        => parseFlowMappingStart()
      case Some('}')                        => parseFlowMappingEnd()
      case Some(',') =>
        in.skipCharacter()
        getNextTokens()
      case Some(_) => fetchValue()
      case None =>
        ctx.checkIndents(-1)
        StreamEnd.token(in.pos)

  private def isDocumentStart =
    in.peekN(3) == "---" && in.peek(3).exists(_.isWhitespace)

  private def parseDocumentStart() =
    in.skipN(4)
    ctx.parseDocumentStart(in.column)

  private def isDocumentEnd =
    in.peekN(3) == "..." && in.peek(3).exists(_.isWhitespace)

  private def parseDocumentEnd() =
    in.skipN(4)
    ctx.parseDocumentEnd()

  private def parseFlowSequenceStart() =
    in.skipCharacter()
    ctx.enterFlowSequence
    Token(FlowSequenceStart, in.pos)

  private def parseFlowSequenceEnd() =
    in.skipCharacter()
    ctx.leaveFlowSequence
    Token(FlowSequenceEnd, in.pos)

  private def parseFlowMappingStart() =
    in.skipCharacter()
    ctx.enterFlowMapping
    Token(FlowMappingStart, in.pos)

  private def parseFlowMappingEnd() =
    in.skipCharacter()
    ctx.leaveFlowMapping
    Token(FlowMappingEnd, in.pos)

  private def parseBlockSequence() =
    val isIn = ctx.isInFlowCollection
    val i    = ctx.indent
    val col  = in.column
    if (!ctx.isInFlowCollection && ctx.indent < in.column) then
      ctx.addIndent(in.column)
      Token(SequenceStart, in.pos)
    else
      in.skipCharacter()
      Token(SequenceValue, in.pos)

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

    val pos = in.pos
    in.skipCharacter() // skip double quote
    val scalar = readScalar()
    Token(Scalar(scalar, ScalarStyle.DoubleQuoted), pos)

  /**
   * This header is followed by a non-content line break with an optional comment.
   */
  private def parseBlockHeader(): Unit =
    while (in.peek() == Some(' ')) {
      in.skipCharacter()
    }

    if in.isNewline then
      in.skipCharacter()
      parseBlockHeader()

  /**
   * final break interpretation - https://yaml.org/spec/1.2/#b-chomped-last(t)
   */
  private def parseChompingIndicator(): BlockChompingIndicator =
    in.peek() match
      case Some('-') =>
        in.skipCharacter()
        BlockChompingIndicator.Strip
      case Some('+') =>
        in.skipCharacter()
        BlockChompingIndicator.Keep
      case _ => BlockChompingIndicator.Clip

  private def parseLiteral(): Token =
    val sb = new StringBuilder

    val pos = in.pos
    in.skipCharacter() // skip |
    val chompingIndicator = parseChompingIndicator()

    parseBlockHeader()

    val foldedIndent = in.column
    skipUntilNextIndent(foldedIndent)

    @tailrec
    def readLiteral(): String =
      in.peek() match
        case Some('\n') =>
          sb.append(in.read())
          skipUntilNextIndent(foldedIndent)
          if (!in.isWhitespace && in.column != foldedIndent) then sb.result()
          else readLiteral()
        case Some(char) =>
          sb.append(in.read())
          readLiteral()
        case None => sb.result()

    val scalar        = readLiteral()
    val chompedScalar = chompingIndicator.removeBlankLinesAtEnd(scalar)
    Token(Scalar(chompedScalar, ScalarStyle.Literal), pos)

  private def parseFoldedValue(): Token =
    val sb = new StringBuilder

    val pos = in.pos
    in.skipCharacter() // skip >
    val chompingIndicator = parseChompingIndicator()

    parseBlockHeader()
    val foldedIndent = in.column
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
            if (!in.isWhitespace && in.column != foldedIndent) then sb.result()
            else
              sb.append(" ")
              readFolded()
          }
        case Some(char) =>
          sb.append(in.read())
          readFolded()
        case None => sb.result()

    val scalar        = readFolded()
    val chompedScalar = chompingIndicator.removeBlankLinesAtEnd(scalar)
    Token(Scalar(chompedScalar, ScalarStyle.Folded), pos)

  private def parseSingleQuoteValue(): Token = {
    val sb = new StringBuilder

    @tailrec
    def readScalar(): String =
      in.peek() match
        case Some('\'') if in.peekNext() == Some('\'') =>
          in.skipN(2)
          sb.append('\'')
          readScalar()
        case Some('\n') =>
          sb.append(' ')
          skipUntilNextChar()
          readScalar()
        case Some('\'') =>
          in.skipCharacter()
          sb.result()
        case Some(char) =>
          sb.append(in.read())
          readScalar()
        case None => sb.result()

    val pos = in.pos
    in.skipCharacter() // skip single quote
    val scalar = readScalar()
    Token(Scalar(scalar, ScalarStyle.SingleQuoted), pos)
  }

  private def parseScalarValue(): Token = {
    val sb           = new StringBuilder
    val scalarIndent = in.column

    def readScalar(): String =
      val peeked = in.peek()
      peeked match
        case Some(':') if in.isNextWhitespace                   => sb.result()
        case Some(char) if !ctx.isAllowedSpecialCharacter(char) => sb.result()
        case Some(' ') if in.peekNext() == Some('#')            => sb.result()
        case _ if in.isNewline =>
          skipUntilNextChar()
          sb.append(' ')
          if (in.column > ctx.indent) readScalar()
          else sb.result()
        case Some(char) =>
          sb.append(in.read())
          readScalar()
        case None => sb.result()

    val pos    = in.pos
    val scalar = readScalar()
    Token(Scalar(scalar.trim, ScalarStyle.Plain), pos)
  }

  private def fetchValue(): Token =
    skipUntilNextToken()
    val peeked = in.peek()
    val scalar: Token = peeked match
      case Some('"')  => parseDoubleQuoteValue()
      case Some('\'') => parseSingleQuoteValue()
      case Some('>')  => parseFoldedValue()
      case Some('|')  => parseLiteral()
      case _          => parseScalarValue()

    val peeked2 = in.peek()
    peeked2 match
      case Some(':') =>
        in.skipCharacter()
        if (ctx.indent < scalar.pos.column) then
          ctx.addIndent(scalar.pos.column)
          ctx.tokens.appendAll(List(Token(MappingStart, scalar.pos)))

        ctx.tokens.appendAll(
          List(
            Token(MappingKey, scalar.pos),
            scalar
          )
        )
        Token(MappingValue, scalar.pos)
      case _ => scalar

  def skipUntilNextToken(): Unit =
    while (in.peek() == Some(' ')) do in.skipCharacter()

    if in.peek() == Some('#') then skipComment()

    if (in.isNewline) then {
      in.skipCharacter()
      skipUntilNextToken()
    }

  def skipUntilNextIndent(indentBlock: Int): Unit =
    while (in.peek() == Some(' ') && in.column < indentBlock) do in.skipCharacter()

  def skipUntilNextChar() =
    while (in.isWhitespace) do in.skipCharacter()

  private def skipComment(): Unit = while !in.isNewline do in.skipCharacter()
}
