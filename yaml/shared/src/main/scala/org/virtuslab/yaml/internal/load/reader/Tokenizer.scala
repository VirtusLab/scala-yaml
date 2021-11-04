package org.virtuslab.yaml.internal.load.reader

import org.virtuslab.yaml.internal.load.reader.token.{BlockChompingIndicator, ScalarStyle, Token}
import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind.*
import org.virtuslab.yaml.internal.load.reader.token.BlockChompingIndicator.*

import scala.util.Try
import scala.annotation.tailrec
import org.virtuslab.yaml.Position

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
    ctx.tokens.appendAll(getNextTokens())
    ctx.tokens.head

  private def getNextTokens(): List[Token] =
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
      case Some('&')                        => parseAnchor()
      case Some(',') =>
        in.skipCharacter()
        List(Token(Comma, in.pos))
      case Some(_) => fetchValue()
      case None =>
        ctx.checkIndents(-1)
        List(Token(StreamEnd, in.pos))

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
    List(Token(FlowSequenceStart, in.pos))

  private def parseFlowSequenceEnd() =
    in.skipCharacter()
    ctx.leaveFlowSequence
    List(Token(FlowSequenceEnd, in.pos))

  private def parseFlowMappingStart() =
    in.skipCharacter()
    ctx.enterFlowMapping
    List(Token(FlowMappingStart, in.pos))

  private def parseFlowMappingEnd() =
    in.skipCharacter()
    ctx.leaveFlowMapping
    List(Token(FlowMappingEnd, in.pos))

  private def parseBlockSequence() =
    if (!ctx.isInFlowCollection && ctx.indent < in.column) then
      ctx.addIndent(in.column)
      List(Token(SequenceStart, in.pos))
    else
      in.skipCharacter()
      List(Token(SequenceValue, in.pos))

  private def parseAnchorName(): (String, Position) =
    val invalidChars = Set('[', ']', '{', '}', ',')
    val sb           = new StringBuilder

    @tailrec
    def readAnchorName(): String =
      in.peek() match
        case Some(char) if !invalidChars(char) && !in.isWhitespace =>
          sb.append(in.read())
          readAnchorName()
        case _ => sb.result()

    val pos = in.pos
    in.skipCharacter()
    val name = readAnchorName()
    (name, pos)

  private def parseAnchor(): List[Token] =
    val (name, anchorPos) = parseAnchorName()
    val nexTokens         = getNextTokens()

    val anchorToken = Token(Anchor(name), anchorPos)
    nexTokens match {
      case Token(_: MappingStart.type, _) :: Token(_: MappingKey.type, _) :: rest =>
        ctx.removeLastIndent()
        ctx.addIndent(anchorPos.column)
        nexTokens.take(2) ::: anchorToken +: rest
      case Token(_: MappingKey.type, _) :: rest if ctx.indent == anchorPos.column =>
        nexTokens.take(1) ::: anchorToken +: rest
      case _ => List(anchorToken) ::: nexTokens
    }

  private def parseAlias() =
    val (name, pos) = parseAnchorName()
    Token(Alias(name), pos)

  private def parseDoubleQuoteValue(): Token =
    val sb = new StringBuilder

    @tailrec
    def readScalar(): String =
      in.peek() match
        case _ if in.isNewline =>
          skipUntilNextChar()
          sb.append(" ")
          readScalar()
        case Some('\\') if in.peekNext() == Some('"') =>
          in.skipN(2)
          sb.append("\"")
          readScalar()
        case Some('"') =>
          in.skipCharacter()
          sb.result()
        case Some(char) =>
          sb.append(in.read())
          readScalar()
        case None =>
          sb.result()

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
      while (in.isNextNewline) {
        in.skipCharacter()
        sb.append("\n")
      }

    @tailrec
    def readFolded(): String =
      in.peek() match
        case _ if in.isNewline =>
          if (in.isNextNewline) {
            chompedEmptyLines()
            if (in.peek().isDefined) then
              in.skipCharacter()
              skipUntilNextIndent(foldedIndent)
            readFolded()
          } else {
            in.skipCharacter()
            skipUntilNextIndent(foldedIndent)
            if (in.column != foldedIndent || in.peek() == None) then sb.result()
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

    def chompedEmptyLines() =
      while (in.isNextNewline) {
        in.skipCharacter()
        sb.append("\n")
      }

    def readScalar(): String =
      val peeked = in.peek()
      peeked match
        case Some(':') if in.isNextWhitespace                   => sb.result()
        case Some(':') if in.peekNext().exists(_ == ',')        => sb.result()
        case Some(char) if !ctx.isAllowedSpecialCharacter(char) => sb.result()
        case _ if isDocumentEnd || isDocumentStart              => sb.result()
        case Some(' ') if in.peekNext() == Some('#')            => sb.result()
        case _ if in.isNewline =>
          if (in.isNextNewline) then chompedEmptyLines()
          else sb.append(' ')
          skipUntilNextChar()
          if (in.column > ctx.indent) readScalar()
          else sb.result()
        case Some(char) =>
          sb.append(in.read())
          readScalar()
        case Some(_) | None => sb.result()

    val pos    = in.pos
    val scalar = readScalar()
    Token(Scalar(scalar.trim, ScalarStyle.Plain), pos)
  }

  private def fetchValue(): List[Token] =
    skipUntilNextToken()
    val peeked = in.peek()
    val scalar: Token = peeked match
      case Some('"')  => parseDoubleQuoteValue()
      case Some('\'') => parseSingleQuoteValue()
      case Some('>')  => parseFoldedValue()
      case Some('|')  => parseLiteral()
      case Some('*')  => parseAlias()
      case _          => parseScalarValue()

    skipUntilNextToken()
    val peeked2 = in.peek()
    peeked2 match
      case Some(':') =>
        in.skipCharacter()
        val maybeMappingStart =
          if (ctx.indent < scalar.pos.column && !ctx.isInFlowCollection) then
            ctx.addIndent(scalar.pos.column)
            List(Token(MappingStart, scalar.pos))
          else Nil

        maybeMappingStart :+ Token(MappingKey, scalar.pos) :+ scalar :+ Token(
          MappingValue,
          scalar.pos
        )
      case _ => List(scalar)

  def skipUntilNextToken(): Unit =
    while (in.isWhitespace) do in.skipCharacter()

    if in.peek() == Some('#') then skipComment()

    if (in.isNewline) then {
      in.skipCharacter()
      skipUntilNextToken()
    }

  def skipUntilNextIndent(indentBlock: Int): Unit =
    while (in.peek() == Some(' ') && in.column < indentBlock) do in.skipCharacter()

  def skipUntilNextChar() =
    while (in.isWhitespace) do in.skipCharacter()

  private def skipComment(): Unit = while (in.peek().isDefined && !in.isNewline) do
    in.skipCharacter()
}
