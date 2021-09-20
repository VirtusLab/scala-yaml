package org.virtuslab.yaml.internal.load.reader

import org.virtuslab.yaml.internal.load.reader.token.{BlockChompingIndicator, ScalarStyle, Token}
import org.virtuslab.yaml.internal.load.reader.token.Token.*
import org.virtuslab.yaml.internal.load.reader.token.BlockChompingIndicator.*

import scala.util.Try
import scala.annotation.tailrec

trait Tokenizer:
  def peekToken(): Token
  def popToken(): Token

private[yaml] class Scanner(str: String) extends Tokenizer {

  private val ctx = ReaderCtx.init(str)
  private val in  = ctx.reader

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
      case None => ctx.closeOpenedScopes() :+ Token.StreamEnd((in.pos()))

  private def isDocumentStart =
    in.peekN(3) == "---" && in.peek(3).exists(_.isWhitespace)

  private def parseDocumentStart(): List[Token] =
    in.skipN(4)
    ctx.parseDocumentStart(in.column)

  private def isDocumentEnd =
    in.peekN(3) == "..." && in.peek(3).exists(_.isWhitespace)

  private def parseDocumentEnd(): List[Token] =
    in.skipN(4)
    ctx.parseDocumentEnd()

  private def parseFlowSequenceStart() =
    in.skipCharacter()
    ctx.appendState(ReaderState.FlowSequence(in.column))
    List(FlowSequenceStart(in.pos()))

  private def parseFlowSequenceEnd() =
    in.skipCharacter()
    ctx.closeOpenedFlowSequence()

  private def parseFlowMappingStart() =
    in.skipCharacter()
    ctx.appendState(ReaderState.FlowMapping(in.column))
    List(FlowMappingStart(in.pos()))

  private def parseFlowMappingEnd() =
    in.skipCharacter()
    ctx.closeOpenedFlowMapping()

  private def parseBlockSequence() =
    ctx.closeOpenedCollectionSequences(in.column)
    if (ctx.shouldParseSequenceEntry(in.column)) then
      in.skipCharacter()
      getNextTokens()
    else
      ctx.appendState(ReaderState.Sequence(in.column))
      List(SequenceStart(in.pos()))

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

    val pos = in.pos()
    in.skipCharacter() // skip double quote
    val scalar = readScalar()
    Scalar(scalar, ScalarStyle.DoubleQuoted, pos)

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

    val pos = in.pos()
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

    val scalar         = readLiteral()
    val choompedScalar = chompingIndicator.removeBlankLinesAtEnd(scalar)
    Scalar(choompedScalar, ScalarStyle.Literal, pos)

  private def parseFoldedValue(): Token =
    val sb = new StringBuilder

    val pos = in.pos()
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

    val scalar         = readFolded()
    val choompedScalar = chompingIndicator.removeBlankLinesAtEnd(scalar)
    Scalar(choompedScalar, ScalarStyle.Folded, pos)

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

    val pos = in.pos()
    in.skipCharacter() // skip single quote
    val scalar = readScalar()
    Scalar(scalar, ScalarStyle.SingleQuoted, pos)
  }

  private def parseScalarValue(): Token = {
    val sb           = new StringBuilder
    val scalarIndent = in.column

    def readScalar(): String =
      in.peek() match
        case Some(':') if in.isNextWhitespace =>
          sb.result()
        case Some(char) if !ctx.isAllowedSpecialCharacter(char) =>
          sb.result()
        case Some(' ') if in.checkNext(c => c == '#' || c == ':') =>
          sb.result()
        case _ if in.isNewline =>
          skipUntilNextChar()
          sb.append(' ')
          if (ctx.getIndentOfLatestCollection().exists(in.column > _)) readScalar()
          else sb.result()
        case Some(char) =>
          sb.append(in.read())
          readScalar()
        case None => sb.result()

    val pos    = in.pos()
    val scalar = readScalar()
    Scalar(scalar.trim, ScalarStyle.Plain, pos)
  }

  private def fetchValue(): List[Token] =
    skipUntilNextToken()
    val scalar: Token = in.peek() match
      case Some('"')  => parseDoubleQuoteValue()
      case Some('\'') => parseSingleQuoteValue()
      case Some('>')  => parseFoldedValue()
      case Some('|')  => parseLiteral()
      case _          => parseScalarValue()

    skipUntilNextToken()
    in.peek() match
      case Some(':') =>
        ctx.closeOpenedCollectionMapping(scalar.pos.column)
        in.skipCharacter()

        if (ctx.shouldParseMappingEntry(scalar.pos.column)) then
          List(Token.Key(scalar.pos), scalar, Token.Value(scalar.pos))
        else if (!ctx.isFlowMapping()) then
          ctx.appendState(ReaderState.Mapping(scalar.pos.column))
          List(MappingStart(scalar.pos), Token.Key(scalar.pos), scalar, Token.Value(scalar.pos))
        else List(scalar)
      case _ => List(scalar)

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
