package org.virtuslab.internal.load.reader

import org.virtuslab.internal.load.reader
import org.virtuslab.internal.load.reader.token.Token
import org.virtuslab.internal.load.reader.token.Token._
import org.virtuslab.internal.load.reader.token.ScalarStyle
import scala.collection.mutable.ArrayDeque

import scala.util.Try
import scala.annotation.tailrec

trait Reader:
  def peekToken(): Token
  def popToken(): Token
  def skipUntilNextToken(): Unit

class YamlReader(in: CharSequence) extends Reader {

  private val ctx    = ReaderCtx.init
  private var indent = 0
  private var offset = 0

  def peekToken(): Token = ctx.tokens.headOption match
    case Some(token) => token
    case None        => getToken()

  def popToken(): Token = ctx.tokens.removeHead()

  private def getToken(): Token =
    ctx.tokens.appendAll(getNextTokens())
    ctx.tokens.head

  @tailrec
  private def getNextTokens(): List[Token] =
    skipUntilNextToken()
    peek() match
      case Some('-') if isDocumentStart  => parseDocumentStart()
      case Some('-') if isNextWhitespace => parseBlockSequence()
      case Some('[') =>
        skipCharacter()
        List(ctx.appendSequence(indent))
      case Some(']') =>
        skipCharacter()
        List(ctx.popTokenFromStack)
      case Some('{') =>
        skipCharacter()
        List(ctx.appendMapping(indent))
      case Some('}') =>
        skipCharacter()
        List(ctx.popTokenFromStack)
      case Some(',') =>
        skipCharacter() // TODO it should return token
        getNextTokens()
      case Some(_) =>
        fetchValue()
      case None =>
        List(ctx.popTokenFromStack)

  private def isNextWhitespace = peekNext().exists(_.isWhitespace)

  private def parseBlockSequence() =
    ctx.closeOpenedCollectionForSequences(indent)
    if (ctx.shouldParseSequenceEntry(indent)) then
      skipCharacter()
      indent += 1
      getNextTokens()
    else
      val token = ctx.appendSequence(indent)
      List(token)

  private def fetchDoubleQuoteValue(): List[Token] = {
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
  }

  private def fetchFoldedValue(): List[Token] = {
    val sb = new StringBuilder

    skipCharacter() // skip >
    peek() match
      case Some('-') =>
        skipCharacter()
      case _ => ()

    skipUntilNextToken()
    val foldedIndent = indent

    @tailrec
    def readScalar(): String =
      peek() match
        case Some('\n') | None =>
          skipUntilNextToken()
          if (indent != foldedIndent) then sb.result()
          else
            sb.append(" ")
            readScalar()
        case Some(char) =>
          sb.append(read())
          readScalar()

    val scalar = readScalar()
    List(Scalar(scalar, ScalarStyle.Folded))
  }

  private def fetchSingleQuoteValue(): List[Token] = {
    val sb = new StringBuilder
    @tailrec
    def readScalar(): String =
      peek() match
        case Some('\'') if peekNext() == Some('\'') =>
          skipCharacter()
          skipCharacter()
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
    ctx.parseDocumentStart(indent)

  private def getScalar(): String = {
    val sb = new StringBuilder
    def readScalar(): String =
      peek() match
        case Some(':')
            if peekNext() == Some(' ') || peekNext() == Some('\n') || peekNext() == Some('\r') =>
          sb.result()
        case Some('\n') | Some('\r') | Some('}') | Some('#') | None => sb.result()
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
          List(Token.Scalar.from(value))
        } else {
          val token = ctx.appendMapping(indent)
          offset = index
          List(token)
        }
      case _ => List(Token.Scalar.from(value))

  private def fetchValue(): List[Token] =
    skipUntilNextToken()

    peek() match
      case Some('"')  => fetchDoubleQuoteValue()
      case Some('\'') => fetchSingleQuoteValue()
      case Some('>')  => fetchFoldedValue()
      case _          => parseScalarValue()

  inline private def peek(n: Int = 0): Option[Char] = Try(in.charAt(offset + n)).toOption
  private def peekNext(): Option[Char]              = peek(1)
  private def peekN(n: Int): String                 = (0 until n).map(peek(_)).flatten.mkString("")

  private def skipCharacter(): Unit = if (offset + 1 > in.length) then () else offset += 1
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

}
