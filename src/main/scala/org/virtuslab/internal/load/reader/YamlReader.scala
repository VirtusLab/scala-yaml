package org.virtuslab.internal.load.reader

import org.virtuslab.internal.load.reader.token.Token
import org.virtuslab.internal.load.reader.token.Token._
import org.virtuslab.internal.load.reader.token.ScalarStyle

import scala.util.Try
import scala.annotation.tailrec

trait Reader:
  def peekToken(): Token
  def popToken(): Token

class YamlReader(in: CharSequence) extends Reader {

  private val ctx    = ReaderCtx.init
  private var indent = 0
  private var offset = 0

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
    peek() match
      case Some('-') if isDocumentStart  => parseDocumentStart()
      case Some('-') if isNextWhitespace => parseBlockSequence()
      case Some('.') if isDocumentEnd    => parseDocumentEnd()
      case Some('[') =>
        skipCharacter()
        ctx.appendState(ReaderState.FlowSequence)
        List(SequenceStart)
      case Some(']') =>
        skipCharacter()
        ctx.closeOpenedSequence()
      case Some('{') =>
        skipCharacter()
        ctx.appendState(ReaderState.FlowMapping)
        List(FlowMappingStart)
      case Some('}') =>
        skipCharacter()
        ctx.closeOpenedFlowMapping()
      case Some(',') =>
        skipCharacter()
        getNextTokens()
      case Some(_) =>
        fetchValue()
      case None =>
        ctx.closeOpenedScopes() :+ Token.StreamEnd

  private def parseBlockSequence() =
    ctx.closeOpenedCollectionSequences(indent)
    if (ctx.shouldParseSequenceEntry(indent)) then
      skipCharacter()
      indent += 1
      getNextTokens()
    else
      ctx.appendState(ReaderState.Sequence(indent))
      List(SequenceStart)

  private def parseDoubleQuoteValue(): List[Token] =
    val sb = new StringBuilder

    @tailrec
    def readScalar(): String =
      peek() match
        case Some('"') | None =>
          skipCharacter()
          sb.result()
        case Some(char) =>
          sb.append(read())
          readScalar()

    skipCharacter() // skip double quote
    val scalar = readScalar()
    List(Scalar(scalar, ScalarStyle.DoubleQuoted))

  private def parseBlockHeader(): Unit =
    while (peek() == Some(' '))
      skipCharacter()

    if peek() == Some('\n') then skipCharacter()

  private def parseLiteral(): List[Token] =
    val sb = new StringBuilder

    skipCharacter() // skip |
    parseBlockHeader()

    val foldedIndent = indent
    skipUntilNextIndent(foldedIndent)

    @tailrec
    def readLiteral(): String =
      peek() match
        case Some('\n') | None =>
          sb.append(escapeSpecialCharacter(read()))

          skipUntilNextIndent(foldedIndent)
          if (indent != foldedIndent) then sb.result()
          else readLiteral()
        case Some(char) =>
          sb.append(escapeSpecialCharacter(read()))
          readLiteral()

    val scalar = readLiteral()
    List(Scalar(scalar, ScalarStyle.Literal))

  private def escapeSpecialCharacter(char: Char): String =
    char match
      case '\\'  => "\\\\"
      case '\n'  => "\\n"
      case other => other.toString

  private def parseFoldedValue(): List[Token] =
    val sb = new StringBuilder

    skipCharacter() // skip >
    peek() match
      case Some('-') =>
        skipCharacter()
      case _ => ()

    val foldedIndent = indent
    parseBlockHeader()
    skipUntilNextIndent(foldedIndent)

    def chompedEmptyLines() =
      while (peekNext() == Some('\n')) {
        skipCharacter()
        sb.append("\\n")
      }

      skipCharacter()
      skipUntilNextIndent(foldedIndent)

    @tailrec
    def readFolded(): String =
      peek() match
        case Some('\n') | None =>
          if (peekNext() == Some('\n') && peek(2) != None) {

            chompedEmptyLines()
            readFolded()
          } else {
            skipCharacter()
            skipUntilNextIndent(foldedIndent)
            if (indent != foldedIndent) then sb.result()
            else
              sb.append(" ")
              readFolded()
          }
        case Some(char) =>
          sb.append(escapeSpecialCharacter(read()))
          readFolded()

    val scalar = readFolded()
    List(Scalar(scalar, ScalarStyle.Folded))

  private def parseSingleQuoteValue(): List[Token] = {
    val sb = new StringBuilder
    @tailrec
    def readScalar(): String =
      peek() match
        case Some('\'') if peekNext() == Some('\'') =>
          skipN(2)
          sb.append('\'')
          readScalar()
        case Some('\'') | None =>
          skipCharacter()
          sb.result()
        case Some(char) =>
          sb.append(read())
          readScalar()

    skipCharacter() // skip single quote
    val scalar = readScalar()
    List(Scalar(scalar, ScalarStyle.SingleQuoted))
  }

  private def isDocumentStart =
    peekN(3) == "---" && peek(3).exists(_.isWhitespace)

  private def parseDocumentStart(): List[Token] =
    skipN(4)
    ctx.parseDocumentStart()

  private def isDocumentEnd =
    peekN(3) == "..." && peek(3).exists(_.isWhitespace)

  private def parseDocumentEnd(): List[Token] =
    skipN(4)
    ctx.parseDocumentEnd()

  private def getScalar(): String = {
    val sb = new StringBuilder
    def readScalar(): String =
      peek() match
        case Some(':')
            if peekNext() == Some(' ') || peekNext() == Some('\n') || peekNext() == Some('\r') =>
          sb.result()
        case Some(char) if !ctx.isAllowedSpecialCharacter(char) => sb.result()
        case Some('\n') | Some('\r') | Some('#') | None         => sb.result()
        case Some(char) =>
          sb.append(read())
          readScalar()

    readScalar().trim
  }

  private def parseScalarValue(): List[Token] =
    val index = offset
    val value = getScalar()

    peek() match
      case Some(':') =>
        ctx.closeOpenedCollectionMapping(indent)

        if (ctx.shouldParseMappingEntry(indent)) {
          skipCharacter()
          List(Token.Key, Token.Scalar.from(value), Token.Value)
        } else if (!ctx.isFlowMapping()) {
          ctx.appendState(ReaderState.Mapping(indent))
          offset = index
          List(MappingStart)
        } else {
          skipCharacter()
          List(Token.Scalar.from(value))
        }
      case _ => List(Token.Scalar.from(value))

  private def fetchValue(): List[Token] =
    skipUntilNextToken()

    peek() match
      case Some('"')  => parseDoubleQuoteValue()
      case Some('\'') => parseSingleQuoteValue()
      case Some('>')  => parseFoldedValue()
      case Some('|')  => parseLiteral()
      case _          => parseScalarValue()

  inline private def peek(n: Int = 0): Option[Char] = Try(in.charAt(offset + n)).toOption
  private def peekNext(): Option[Char]              = peek(1)
  private def peekN(n: Int): String                 = (0 until n).map(peek(_)).flatten.mkString("")
  private def isNextWhitespace                      = peekNext().exists(_.isWhitespace)

  private def skipCharacter(): Unit = offset += 1
  private def skipN(n: Int): Unit   = (1 to n).foreach(_ => skipCharacter())

  private def skipComment(): Unit =
    while !(peek() == Some('\n') || peek() == Some('\r')) do skipCharacter()

  private def read(): Char =
    offset += 1
    in.charAt(offset - 1)

  def skipUntilNextToken(): Unit =
    while (peek() == Some(' ')) {
      indent += 1
      skipCharacter()
    }

    if peek() == Some('#') then skipComment()

    if (peek() == Some('\n') || peek() == Some('\r')) then {
      skipCharacter()
      indent = 0
      skipUntilNextToken()
    }

  def skipUntilNextIndent(indentBlock: Int): Unit =
    indent = 0
    while (peek() == Some(' ') && indent < indentBlock) {
      indent += 1
      skipCharacter()
    }

}
