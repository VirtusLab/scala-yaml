package org.virtuslab.yaml.internal.load.reader

import scala.annotation.tailrec
import scala.util.Try

import org.virtuslab.yaml.Range
import org.virtuslab.yaml.ScannerError
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.reader.token.BlockChompingIndicator
import org.virtuslab.yaml.internal.load.reader.token.BlockChompingIndicator.*
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind.*

import org.virtuslab.yaml.internal.load.TagHandle
import org.virtuslab.yaml.internal.load.TagPrefix
import org.virtuslab.yaml.internal.load.TagValue
import org.virtuslab.yaml.internal.load.TagSuffix
import org.virtuslab.yaml.internal.load.reader.token.TokenKind

trait Tokenizer:
  def peekToken(): Either[YamlError, Token]
  def popToken(): Token

private[yaml] class Scanner(str: String) extends Tokenizer {

  private val ctx = ReaderCtx(str)
  private val in  = ctx.reader

  override def peekToken(): Either[YamlError, Token] = ctx.tokens.headOption match
    case Some(token) => Right(token)
    case None =>
      try {
        Right(getToken())
      } catch {
        case e: ScannerError => Left(e)
      }

  override def popToken(): Token = ctx.tokens.removeHead()

  private def getToken(): Token =
    ctx.tokens.appendAll(getNextTokens())
    ctx.tokens.head

  private def getNextTokens(): List[Token] =
    skipUntilNextToken()
    val closedTokens = ctx.checkIndents(in.column)
    val peeked       = in.peek()
    val tokens: List[Token] = peeked match
      case Some('-') if isDocumentStart     => parseDocumentStart()
      case Some('-') if in.isNextWhitespace => parseBlockSequence()
      case Some('.') if isDocumentEnd       => parseDocumentEnd()
      case Some('[')                        => parseFlowSequenceStart()
      case Some(']')                        => parseFlowSequenceEnd()
      case Some('{')                        => parseFlowMappingStart()
      case Some('}')                        => parseFlowMappingEnd()
      case Some('&')                        => parseAnchor()
      case Some('!')                        => parseTag()
      case Some('%')                        => parseDirective()
      case Some(',') =>
        in.skipCharacter()
        List(Token(Comma, in.range))
      case Some(_) => fetchValue()
      case None =>
        ctx.checkIndents(-1) ++ List(Token(StreamEnd, in.range))

    closedTokens ++ tokens

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
    List(Token(FlowSequenceStart, in.range))

  private def parseFlowSequenceEnd() =
    in.skipCharacter()
    ctx.leaveFlowSequence
    List(Token(FlowSequenceEnd, in.range))

  private def parseFlowMappingStart() =
    in.skipCharacter()
    ctx.enterFlowMapping
    List(Token(FlowMappingStart, in.range))

  private def parseFlowMappingEnd() =
    in.skipCharacter()
    ctx.leaveFlowMapping
    List(Token(FlowMappingEnd, in.range))

  private def parseBlockSequence() =
    if (!ctx.isInFlowCollection && ctx.indent < in.column) then
      ctx.addIndent(in.column)
      List(Token(SequenceStart, in.range))
    else
      in.skipCharacter()
      List(Token(SequenceValue, in.range))

  private def parseDirective(): List[Token] = {
    val range = in.range
    in.skipCharacter() // skip %

    def parseYamlDirective() = { ??? }

    def parseTagDirective() = {
      def parseTagHandle() = {
        val next = in.peekNext()
        in.peek() match
          case Some('!') if next.contains(' ') =>
            in.skipCharacter() // skip exclamation mark
            TagHandle.Primary
          case Some('!') if next.contains('!') =>
            in.skipN(2) // skip both exclamation marks
            TagHandle.Secondary
          case Some('!') =>
            val sb = new StringBuilder
            sb.append(in.read())
            while (in.peek().exists(c => !c.isWhitespace && c != '!')) do sb.append(in.read())
            sb.append(in.read())
            TagHandle.Named(sb.result())
          case _ => ???
      }

      def parseTagPrefix() = {
        skipSpaces()
        in.peek() match
          case Some('!') =>
            val sb = new StringBuilder
            while (in.peek().exists(c => !c.isWhitespace)) do sb.append(in.read())
            TagPrefix.Local(sb.result())
          case Some(char) if char != '!' && char != ',' =>
            val sb = new StringBuilder
            while (in.peek().exists(c => !c.isWhitespace)) do sb.append(in.read())
            TagPrefix.Global(sb.result())
          case _ => ???
      }

      skipSpaces()
      in.peek() match
        case Some('!') =>
          val handle = parseTagHandle()
          val prefix = parseTagPrefix()
          List(Token(TokenKind.TagDirective(handle, prefix), range))
        case _ => ???
    }

    in.peek() match
      case Some('Y') if in.peekN(4) == "YAML" =>
        in.skipN(4)
        parseYamlDirective()
      case Some('T') if in.peekN(3) == "TAG" =>
        in.skipN(3)
        parseTagDirective()
      case _ => ???
  }

  private def parseTag() =
    val range        = in.range
    val invalidChars = Set('[', ']', '{', '}')

    def parseVerbatimTag(): String =
      val sb = new StringBuilder
      sb.append('!')
      while (in.peek().exists(c => c != '>' && !c.isWhitespace)) do sb.append(in.read())
      in.peek() match
        case Some('>') =>
          sb.append(in.read())
          sb.result()
        case _ => ??? // error

    def parseTagSuffix(): String =
      val sb = new StringBuilder
      while (in.peek().exists(c => !invalidChars(c) && !c.isWhitespace)) do sb.append(in.read())
      sb.result()

    def parseShorthandTag(second: Char): TagValue =
      second match
        case '!' => // tag handle starts with '!!'
          in.skipCharacter()
          TagValue.Shorthand(TagHandle.Secondary, parseTagSuffix())
        case _ => // tag handle starts with '!<char>' where char isn't space
          val sb = new StringBuilder

          while (in.peek().exists(c => !c.isWhitespace && c != '!')) do sb.append(in.read())
          in.peek() match
            case Some('!') =>
              sb.insert(0, '!')    // prepend already skipped exclamation mark
              sb.append(in.read()) // append ending exclamation mark
              TagValue.Shorthand(TagHandle.Named(sb.result()), parseTagSuffix())
            case Some(' ') =>
              TagValue.Shorthand(TagHandle.Primary, sb.result())
            case _ => ???

    in.skipCharacter() // skip first '!'
    val peeked = in.peek()
    val tag: Tag = peeked match
      case Some('<') =>
        val tag = parseVerbatimTag()
        Tag(TagValue.Verbatim(tag))
      case Some(' ') =>
        Tag(TagValue.NonSpecific)
      case Some(char) =>
        val tagValue = parseShorthandTag(char)
        Tag(tagValue)
      case None => ??? // error

    List(Token(tag, range))

  private def parseAnchorName(): (String, Range) =
    val invalidChars = Set('[', ']', '{', '}', ',')
    val sb           = new StringBuilder

    @tailrec
    def readAnchorName(): String =
      in.peek() match
        case Some(char) if !invalidChars(char) && !in.isWhitespace =>
          sb.append(in.read())
          readAnchorName()
        case _ => sb.result()

    val range = in.range
    in.skipCharacter()
    val name = readAnchorName()
    (name, range)

  private def parseAnchor(): List[Token] =
    val (name, anchorPos) = parseAnchorName()
    val nexTokens         = getNextTokens()

    val anchorToken = Token(Anchor(name), anchorPos)
    nexTokens match {
      case (Token(_: MappingStart.type, _) |
          Token(_: FlowMappingStart.type, _)) :: Token(_: MappingKey.type, _) :: rest =>
        ctx.removeLastIndent()
        ctx.addIndent(anchorPos.start.column)
        nexTokens.take(2) ::: anchorToken +: rest
      case Token(_: MappingKey.type, _) :: rest
          if (ctx.indent == anchorPos.start.column || ctx.isInFlowCollection) =>
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

    val range = in.range
    in.skipCharacter() // skip double quote
    val scalar   = readScalar()
    val endRange = range.withEndPos(in.pos)
    Token(Scalar(scalar, ScalarStyle.DoubleQuoted), endRange)

  /**
   * This header is followed by a non-content line break with an optional comment.
   */
  private def parseBlockHeader(): Unit =
    while (in.peek() == Some(' ')) {
      in.skipCharacter()
    }

    if (in.peek() == Some('#'))
      skipComment()

    if in.isNewline then in.skipCharacter()

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

  private def parseIndentationIndicator(): Option[Int] =
    in.peek() match
      case Some(number) if number.isDigit =>
        in.skipCharacter()
        Some(number.asDigit)
      case _ => None

  private def parseLiteral(): Token =
    val sb = new StringBuilder

    val range = in.range
    in.skipCharacter() // skip |
    val indentationIndicator: Option[Int] = parseIndentationIndicator()
    val chompingIndicator                 = parseChompingIndicator()
    val indentation =
      if (indentationIndicator.isEmpty) parseIndentationIndicator() else indentationIndicator

    parseBlockHeader()
    if (indentation.isEmpty) skipUntilNextChar()

    val foldedIndent = indentation.getOrElse(in.column)
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
    Token(Scalar(chompedScalar, ScalarStyle.Literal), range)

  private def parseFoldedValue(): Token =
    val sb = new StringBuilder

    val range = in.range
    in.skipCharacter() // skip >
    val indentationIndicator: Option[Int] = parseIndentationIndicator()
    val chompingIndicator                 = parseChompingIndicator()
    val indentation =
      if (indentationIndicator.isEmpty) parseIndentationIndicator() else indentationIndicator

    parseBlockHeader()
    if (indentation.isEmpty) skipUntilNextChar()
    val foldedIndent = indentation.getOrElse(in.column)
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
            if (in.column != foldedIndent || in.peek() == None) then {
              sb.append("\n")
              sb.result()
            } else
              sb.append(" ")
              readFolded()
          }
        case Some(char) =>
          sb.append(in.read())
          readFolded()
        case None => sb.result()

    val scalar        = readFolded()
    val chompedScalar = chompingIndicator.removeBlankLinesAtEnd(scalar)
    Token(Scalar(chompedScalar, ScalarStyle.Folded), range)

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

    val range = in.range
    in.skipCharacter() // skip single quote
    val scalar   = readScalar()
    val endRange = range.withEndPos(in.pos)
    Token(Scalar(scalar, ScalarStyle.SingleQuoted), endRange)
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

    val range    = in.range
    val scalar   = readScalar()
    val endRange = range.withEndPos(in.pos)
    Token(Scalar(scalar.trim, ScalarStyle.Plain), endRange)
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
          if (ctx.indent < scalar.range.start.column && !ctx.isInFlowCollection) then
            ctx.addIndent(scalar.range.start.column)
            List(Token(MappingStart, scalar.range))
          else Nil

        val mappingValueToken = Token(MappingValue, scalar.range)

        if (scalar.range.end.exists(_.line > scalar.range.start.line) && !ctx.isInFlowCollection) {
          throw ScannerError.from("Not alowed here a mapping value", mappingValueToken)
        } else
          maybeMappingStart ++ List(
            Token(MappingKey, scalar.range),
            scalar,
            mappingValueToken
          )
      case _ => List(scalar)

  def skipUntilNextToken(): Unit =
    while (in.isWhitespace) do in.skipCharacter()

    if in.peek() == Some('#') then skipComment()

    if (in.isNewline) then {
      in.skipCharacter()
      skipUntilNextToken()
    }

  def skipSpaces(): Unit =
    while (in.peek().contains(' ')) do in.skipCharacter()

  def skipUntilNextIndent(indentBlock: Int): Unit =
    while (in.peek() == Some(' ') && in.column < indentBlock) do in.skipCharacter()

  def skipUntilNextChar() =
    while (in.isWhitespace) do in.skipCharacter()

  private def skipComment(): Unit = while (in.peek().isDefined && !in.isNewline) do
    in.skipCharacter()
}
