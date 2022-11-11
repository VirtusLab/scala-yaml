package org.virtuslab.yaml.internal.load.reader

import java.net.URLDecoder
import java.nio.charset.StandardCharsets

import scala.annotation.tailrec
import scala.util.Try

import org.virtuslab.yaml.Range
import org.virtuslab.yaml.ScannerError
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.TagHandle
import org.virtuslab.yaml.internal.load.TagPrefix
import org.virtuslab.yaml.internal.load.TagSuffix
import org.virtuslab.yaml.internal.load.TagValue
import org.virtuslab.yaml.internal.load.reader.token.BlockChompingIndicator
import org.virtuslab.yaml.internal.load.reader.token.BlockChompingIndicator._
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind
import org.virtuslab.yaml.internal.load.reader.token.TokenKind._

trait Tokenizer {
  def peekToken(): Either[YamlError, Token]
  def popToken(): Token
}

private[yaml] class Scanner(str: String) extends Tokenizer {

  private val ctx = ReaderCtx(str)
  private val in  = ctx.reader

  override def peekToken(): Either[YamlError, Token] = ctx.tokens.headOption match {
    case Some(token) => Right(token)
    case None =>
      try {
        Right(getToken())
      } catch {
        case e: ScannerError => Left(e)
      }
  }

  override def popToken(): Token = ctx.tokens.removeHead()

  private def getToken(): Token = {
    while (ctx.needMoreTokens())
      ctx.tokens.appendAll(getNextTokens())
    ctx.tokens.head
  }

  /**
  * Plain keys have to be resolved in the same line they were created, otherwise they cannot be keys, thus they are ordinary tokens
  */
  private def shouldPopPlainKeys: Boolean =
    !ctx.isInFlowCollection && ctx.potentialKeys.headOption
      .exists(_.range.start.line != in.line)

  private def getNextTokens(): List[Token] = {
    skipUntilNextToken()
    val closedBlockTokens = ctx.checkIndents(in.column)
    val closedTokens =
      if (closedBlockTokens.nonEmpty || shouldPopPlainKeys)
        ctx.popPotentialKeys() ++ closedBlockTokens
      else closedBlockTokens
    val peeked = in.peek()
    val tokens: List[Token] = peeked match {
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
      case Some('"')                        => parseDoubleQuoteValue()
      case Some('\'')                       => parseSingleQuoteValue()
      case Some('>')                        => parseFoldedValue()
      case Some('|')                        => parseLiteral()
      case Some('*')                        => parseAlias()
      case Some(',') =>
        in.skipCharacter()
        ctx.isPlainKeyAllowed = true
        ctx.popPotentialKeys() ++ List(Token(Comma, in.range))
      case Some(':')
          if (in.isNextWhitespace || (ctx.isInFlowCollection && ctx.isPlainKeyAllowed)) =>
        fetchValue()
      case Some(_) => parsePlainScalar()
      case None =>
        ctx.popPotentialKeys() ++ ctx.checkIndents(-1) ++ List(Token(StreamEnd, in.range))
    }
    closedTokens ++ tokens
  }

  private def isDocumentStart =
    in.peekN(3) == "---" && in.peek(3).exists(_.isWhitespace)

  private def parseDocumentStart() = {
    in.skipN(4)
    ctx.parseDocumentStart(in.column)
  }

  private def isDocumentEnd =
    in.peekN(3) == "..." && in.peek(3).exists(_.isWhitespace)

  private def parseDocumentEnd() = {
    in.skipN(4)
    ctx.parseDocumentEnd()
  }

  private def parseFlowSequenceStart() = {
    in.skipCharacter()
    ctx.enterFlowSequence
    ctx.popPotentialKeys() ++ List(Token(FlowSequenceStart, in.range))
  }

  private def parseFlowSequenceEnd() = {
    in.skipCharacter()
    ctx.leaveFlowSequence
    ctx.popPotentialKeys() ++ List(Token(FlowSequenceEnd, in.range))
  }

  private def parseFlowMappingStart() = {
    in.skipCharacter()
    ctx.enterFlowMapping
    ctx.isPlainKeyAllowed = true
    ctx.popPotentialKeys() ++ List(Token(FlowMappingStart, in.range))
  }

  private def parseFlowMappingEnd() = {
    in.skipCharacter()
    ctx.leaveFlowMapping
    ctx.popPotentialKeys() ++ List(Token(FlowMappingEnd, in.range))
  }

  private def parseBlockSequence() =
    if (!ctx.isInFlowCollection && ctx.indent < in.column) {
      ctx.addIndent(in.column)
      List(Token(SequenceStart, in.range))
    } else if (ctx.isInBlockCollection && !ctx.isPlainKeyAllowed) {
      val range = in.range
      throw ScannerError(
        s"line ${range.start.line}, column ${range.start.column} - cannot start sequence here"
      )
    } else {
      in.skipCharacter()
      ctx.popPotentialKeys() ++ List(Token(SequenceValue, in.range))
    }

  private def parseDirective(): List[Token] = {
    val range = in.range
    in.skipCharacter() // skip %

    def parseYamlDirective() = { throw ScannerError("YAML directives are not supported yet.") }

    def parseTagDirective() = {
      def parseTagHandle() = {
        in.peekNext() match { // peeking next char!! current char is exclamation mark
          case Some(' ') =>
            in.skipCharacter() // skip exclamation mark
            TagHandle.Primary
          case Some('!') =>
            in.skipN(2) // skip both exclamation marks
            TagHandle.Secondary
          case _ =>
            val sb = new StringBuilder
            sb.append(in.read())
            while (in.peek().exists(c => !c.isWhitespace && c != '!')) sb.append(in.read())
            sb.append(in.read())
            TagHandle.Named(sb.result())
        }
      }

      def parseTagPrefix() = {
        skipSpaces()
        in.peek() match {
          case Some('!') =>
            val sb = new StringBuilder
            while (in.peek().exists(c => !c.isWhitespace)) sb.append(in.read())
            TagPrefix.Local(sb.result())
          case Some(char) if char != '!' && char != ',' =>
            val sb = new StringBuilder
            while (in.peek().exists(c => !c.isWhitespace)) sb.append(in.read())
            TagPrefix.Global(sb.result())
          case _ => throw ScannerError("Invalid tag prefix in TAG directive")
        }
      }

      skipSpaces()
      in.peek() match {
        case Some('!') =>
          val handle = parseTagHandle()
          val prefix = parseTagPrefix()
          List(Token(TokenKind.TagDirective(handle, prefix), range))
        case _ => throw ScannerError("Tag handle in TAG directive should start with '!'")
      }
    }

    in.peek() match {
      case Some('Y') if in.peekN(4) == "YAML" =>
        in.skipN(4)
        parseYamlDirective()
      case Some('T') if in.peekN(3) == "TAG" =>
        in.skipN(3)
        parseTagDirective()
      case _ => throw ScannerError("Unknown directive, expected YAML or TAG")
    }
  }

  private def parseTag() = {
    val range        = in.range
    val invalidChars = Set('[', ']', '{', '}')

    def parseVerbatimTag(): String = {
      val sb = new StringBuilder
      sb.append('!')
      while (in.peek().exists(c => c != '>' && !c.isWhitespace)) sb.append(in.read())
      in.peek() match {
        case Some('>') =>
          sb.append(in.read())
          sb.result()
        case _ => throw ScannerError("Lacks '>' which closes verbatim tag attribute")
      }
    }

    def parseTagSuffix(): String = {
      val sb = new StringBuilder
      while (in.peek().exists(c => !invalidChars(c) && !c.isWhitespace)) sb.append(in.read())
      if (in.peek().exists(c => invalidChars(c))) throw ScannerError("Invalid character in tag")
      UrlDecoder.decode(sb.result())
    }

    def parseShorthandTag(second: Char): TagValue =
      second match {
        case '!' => // tag handle starts with '!!'
          in.skipCharacter()
          TagValue.Shorthand(TagHandle.Secondary, parseTagSuffix())
        case _ => // tag handle starts with '!<char>' where char isn't space
          val sb = new StringBuilder

          while (in.peek().exists(c => !invalidChars(c) && !c.isWhitespace && c != '!'))
            sb.append(in.read())
          if (in.peek().exists(c => invalidChars(c)))
            throw ScannerError("Invalid character in tag")
          in.peek() match {
            case Some('!') =>
              sb.insert(0, '!')    // prepend already skipped exclamation mark
              sb.append(in.read()) // append ending exclamation mark
              TagValue.Shorthand(TagHandle.Named(sb.result()), parseTagSuffix())
            case Some(' ') =>
              TagValue.Shorthand(TagHandle.Primary, sb.result())
            case _ => throw ScannerError("Invalid tag handle")
          }
      }

    in.skipCharacter() // skip first '!'
    val peeked = in.peek()
    val tag: Tag = peeked match {
      case Some('<') =>
        val tag = parseVerbatimTag()
        Tag(TagValue.Verbatim(tag))
      case Some(' ') =>
        Tag(TagValue.NonSpecific)
      case Some(char) =>
        val tagValue = parseShorthandTag(char)
        Tag(tagValue)
      case None => throw ScannerError("Input stream ended unexpectedly")
    }

    if (ctx.isPlainKeyAllowed) {
      ctx.potentialKeys.append(Token(tag, range))
      Nil
    } else List(Token(tag, range))
  }

  private def parseAnchorName(): (String, Range) = {
    val invalidChars = Set('[', ']', '{', '}', ',')
    val sb           = new StringBuilder

    @tailrec
    def readAnchorName(): String =
      in.peek() match {
        case Some(char) if !invalidChars(char) && !in.isWhitespace =>
          sb.append(in.read())
          readAnchorName()
        case _ => sb.result()
      }

    val range = in.range
    in.skipCharacter()
    val name = readAnchorName()
    (name, range)
  }

  private def parseAnchor(): List[Token] = {
    val (name, range) = parseAnchorName()
    val anchorToken   = Token(Anchor(name), range)
    if (ctx.isPlainKeyAllowed) {
      ctx.potentialKeys.append(anchorToken)
      Nil
    } else List(anchorToken)
  }

  private def parseAlias(): List[Token] = {
    val (name, pos) = parseAnchorName()
    val aliasToken  = Token(Alias(name), pos)
    if (ctx.isPlainKeyAllowed) {
      ctx.potentialKeys.append(aliasToken)
      Nil
    } else List(aliasToken)
  }

  private def parseDoubleQuoteValue(): List[Token] = {
    val sb = new StringBuilder

    @tailrec
    def readScalar(): String =
      in.peek() match {
        case _ if in.isNewline =>
          skipUntilNextToken()
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
      }

    val isPlainKeyAllowed = ctx.isPlainKeyAllowed
    val range             = in.range
    in.skipCharacter() // skip double quote
    val scalar      = readScalar()
    val endRange    = range.withEndPos(in.pos)
    val scalarToken = Token(Scalar(scalar, ScalarStyle.DoubleQuoted), endRange)
    if (isPlainKeyAllowed) {
      ctx.potentialKeys.append(scalarToken)
      Nil
    } else List(scalarToken)
  }

  /**
   * This header is followed by a non-content line break with an optional comment.
   */
  private def parseBlockHeader(): Unit = {
    while (in.peek() == Some(' '))
      in.skipCharacter()

    if (in.peek() == Some('#'))
      skipComment()

    if (in.isNewline) in.skipCharacter()
  }

  /**
   * final break interpretation - https://yaml.org/spec/1.2/#b-chomped-last(t)
   */
  private def parseChompingIndicator(): BlockChompingIndicator =
    in.peek() match {
      case Some('-') =>
        in.skipCharacter()
        BlockChompingIndicator.Strip
      case Some('+') =>
        in.skipCharacter()
        BlockChompingIndicator.Keep
      case _ => BlockChompingIndicator.Clip
    }

  private def parseIndentationIndicator(): Option[Int] =
    in.peek() match {
      case Some(number) if number.isDigit =>
        in.skipCharacter()
        Some(number.asDigit)
      case _ => None
    }

  private def parseLiteral(): List[Token] = {
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
      in.peek() match {
        case _ if in.isNewline =>
          ctx.isPlainKeyAllowed = true
          sb.append(in.read())
          skipUntilNextIndent(foldedIndent)
          if (!in.isWhitespace && in.column != foldedIndent) sb.result()
          else readLiteral()
        case Some(char) =>
          sb.append(in.read())
          readLiteral()
        case None => sb.result()
      }

    val scalar        = readLiteral()
    val chompedScalar = chompingIndicator.removeBlankLinesAtEnd(scalar)
    List(Token(Scalar(chompedScalar, ScalarStyle.Literal), range))
  }

  private def parseFoldedValue(): List[Token] = {
    val sb = new StringBuilder

    val range = in.range
    in.skipCharacter() // skip >
    val indentationIndicator: Option[Int] = parseIndentationIndicator()
    val chompingIndicator                 = parseChompingIndicator()
    val indentation =
      if (indentationIndicator.isEmpty) parseIndentationIndicator() else indentationIndicator

    parseBlockHeader()
    if (indentation.isEmpty) skipUntilNextToken()
    val foldedIndent = indentation.getOrElse(in.column)
    skipUntilNextIndent(foldedIndent)

    def chompedEmptyLines() =
      while (in.isNextNewline) {
        in.skipCharacter()
        sb.append("\n")
      }

    @tailrec
    def readFolded(): String =
      in.peek() match {
        case _ if in.isNewline =>
          ctx.isPlainKeyAllowed = true
          if (in.isNextNewline) {
            chompedEmptyLines()
            if (in.peek().isDefined) {
              in.skipCharacter()
              skipUntilNextIndent(foldedIndent)
            }
            readFolded()
          } else {
            in.skipCharacter()
            skipUntilNextIndent(foldedIndent)
            if (in.column != foldedIndent || in.peek() == None) {
              sb.append("\n")
              sb.result()
            } else {
              sb.append(" ")
              readFolded()
            }
          }
        case Some(char) =>
          sb.append(in.read())
          readFolded()
        case None => sb.result()
      }

    val scalar        = readFolded()
    val chompedScalar = chompingIndicator.removeBlankLinesAtEnd(scalar)
    List(Token(Scalar(chompedScalar, ScalarStyle.Folded), range))
  }

  private def parseSingleQuoteValue(): List[Token] = {
    val sb = new StringBuilder

    @tailrec
    def readScalar(): String =
      in.peek() match {
        case Some('\'') if in.peekNext() == Some('\'') =>
          in.skipN(2)
          sb.append('\'')
          readScalar()
        case Some('\n') =>
          sb.append(' ')
          skipUntilNextToken()
          readScalar()
        case Some('\'') =>
          in.skipCharacter()
          sb.result()
        case Some(char) =>
          sb.append(in.read())
          readScalar()
        case None => sb.result()
      }

    val isPlainKeyAllowed = ctx.isPlainKeyAllowed
    val range             = in.range
    in.skipCharacter() // skip single quote
    val scalar      = readScalar()
    val endRange    = range.withEndPos(in.pos)
    val scalarToken = Token(Scalar(scalar, ScalarStyle.SingleQuoted), endRange)
    if (isPlainKeyAllowed) {
      ctx.potentialKeys.append(scalarToken)
      Nil
    } else List(scalarToken)
  }

  private def parsePlainScalar(): List[Token] = {
    val sb           = new StringBuilder
    val scalarIndent = in.column

    def chompedEmptyLines() =
      while (in.isNextNewline) {
        in.skipCharacter()
        sb.append("\n")
      }

    def readScalar(): String = {
      val peeked = in.peek()
      peeked match {
        case Some(':') if in.isNextWhitespace                                      => sb.result()
        case Some(':') if in.peekNext().exists(_ == ',') && ctx.isInFlowCollection => sb.result()
        case Some(char) if !ctx.isAllowedSpecialCharacter(char)                    => sb.result()
        case _ if isDocumentEnd || isDocumentStart                                 => sb.result()
        case Some(' ') if in.peekNext() == Some('#')                               => sb.result()
        case _ if in.isNewline =>
          ctx.isPlainKeyAllowed = true
          if (in.isNextNewline) chompedEmptyLines()
          else sb.append(' ')
          skipUntilNextToken()
          if (in.column > ctx.indent)
            readScalar()
          else sb.result()
        case Some(char) =>
          sb.append(in.read())
          readScalar()
        case Some(_) | None => sb.result()
      }
    }

    val isPlainKeyAllowed = ctx.isPlainKeyAllowed
    val range             = in.range
    val scalar            = readScalar()
    val endRange          = range.withEndPos(in.pos)
    val scalarToken       = Token(Scalar(scalar.trim, ScalarStyle.Plain), endRange)
    if (isPlainKeyAllowed) {
      ctx.potentialKeys.append(scalarToken)
      Nil
    } else List(scalarToken)
  }

  private def fetchValue(): List[Token] = {
    in.skipCharacter() // skip
    val mappingValueToken = Token(MappingValue, in.range)

    lazy val firstSimpleKey = ctx.potentialKeys.headOption.getOrElse(
      throw ScannerError.from("Not found expected key for value", mappingValueToken)
    )

    val maybeMappingStart =
      if (!ctx.isInFlowCollection && ctx.indent < firstSimpleKey.range.start.column) {
        ctx.addIndent(firstSimpleKey.range.start.column)
        List(Token(MappingStart, firstSimpleKey.range))
      } else Nil

    val potentialKeys = ctx.popPotentialKeys()
    ctx.isPlainKeyAllowed = false

    if (
      ctx.isInBlockCollection &&
      firstSimpleKey.range.end.exists(
        _.line > firstSimpleKey.range.start.line
      )
    )
      throw ScannerError.from("Not alowed here a mapping value", mappingValueToken)
    else
      maybeMappingStart ++ List(Token(MappingKey, in.range)) ++ potentialKeys :+ mappingValueToken
  }

  def skipUntilNextToken(): Unit = {
    while (in.isWhitespace && !in.isNewline) in.skipCharacter()

    if (in.peek() == Some('#')) skipComment()

    if (in.isNewline) {
      ctx.isPlainKeyAllowed = true
      in.skipCharacter()
      skipUntilNextToken()
    }
  }

  def skipSpaces(): Unit =
    while (in.peek().contains(' ')) in.skipCharacter()

  def skipUntilNextIndent(indentBlock: Int): Unit =
    while (in.peek() == Some(' ') && in.column < indentBlock) in.skipCharacter()

  def skipUntilNextChar() =
    while (in.isWhitespace) in.skipCharacter()

  private def skipComment(): Unit = while (in.peek().isDefined && !in.isNewline)
    in.skipCharacter()
}
