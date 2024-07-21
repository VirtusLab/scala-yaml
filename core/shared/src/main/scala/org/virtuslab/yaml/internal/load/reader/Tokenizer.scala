package org.virtuslab.yaml.internal.load.reader

import java.net.URLDecoder
import java.nio.charset.StandardCharsets

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
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

object Tokenizer {
  def make(str: String): Tokenizer = new StringTokenizer(str)
}

private class StringTokenizer(str: String) extends Tokenizer {

  private val ctx = TokenizerContext(str)
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
  * Plain keys have to be resolved in the same line they were created, otherwise they are ordinary tokens.
  */
  private def shouldPopPlainKeys: Boolean =
    ctx.isInBlockCollection && ctx.potentialKeyOpt
      .exists(_.range.start.line != in.line)

  private def getNextTokens(): Seq[Token] = {
    skipUntilNextToken()
    val closedBlockTokens = ctx.checkIndents(in.column)
    val closedTokens =
      if (closedBlockTokens.nonEmpty || shouldPopPlainKeys)
        ctx.popPotentialKeys() ++ closedBlockTokens
      else closedBlockTokens
    val peeked = in.peek()
    val tokens: List[Token] = peeked match {
      case Reader.nullTerminator =>
        ctx.popPotentialKeys() ++ ctx.checkIndents(-1) ++ List(Token(StreamEnd, in.range))
      case '-' if isDocumentStart     => parseDocumentStart()
      case '-' if in.isNextWhitespace => parseBlockSequence()
      case '.' if isDocumentEnd       => parseDocumentEnd()
      case '['                        => parseFlowSequenceStart()
      case ']'                        => parseFlowSequenceEnd()
      case '{'                        => parseFlowMappingStart()
      case '}'                        => parseFlowMappingEnd()
      case '&'                        => parseAnchor()
      case '!'                        => parseTag()
      case '%'                        => parseDirective()
      case '"'                        => parseDoubleQuoteValue()
      case '\''                       => parseSingleQuoteValue()
      case '>'                        => parseFoldedValue()
      case '|'                        => parseLiteral()
      case '*'                        => parseAlias()
      case ',' =>
        in.skipCharacter()
        ctx.isPlainKeyAllowed = true
        ctx.popPotentialKeys() ++ List(Token(Comma, in.range))
      case ':' if (in.isNextWhitespace || (ctx.isInFlowCollection && ctx.isPlainKeyAllowed)) =>
        fetchValue()
      case _ => parsePlainScalar()
    }
    closedTokens ++ tokens
  }

  private def isDocumentStart =
    in.peekN(3) == "---" && in.peek(3).isWhitespace

  private def parseDocumentStart() = {
    in.skipN(4)
    ctx.parseDocumentStart(in.column)
  }

  private def isDocumentEnd =
    in.peekN(3) == "..." && in.peek(3).isWhitespace

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

  private def parseBlockSequence() = {
    val builder = new ListBuffer[Token]

    // when last indent is lesser than current one, it means that this is start of the sequence
    if (ctx.isInBlockCollection && ctx.indent < in.column) {
      ctx.addIndent(in.column)
      builder.addOne(Token(SequenceStart, in.range))
    }

    if (ctx.isInBlockCollection && !ctx.isPlainKeyAllowed) {
      throw ScannerError.from(in.range, "cannot start sequence")
    }

    in.skipCharacter() // skip '-'
    builder.addAll(ctx.popPotentialKeys())
    builder.addOne(Token(SequenceValue, in.range))

    builder.toList
  }

  private def parseDirective(): List[Token] = {
    val range = in.range
    in.skipCharacter() // skip %

    def parseYamlDirective() = {
      throw ScannerError.from(in.range, "YAML directives are not supported yet.")
    }

    def parseTagDirective() = {
      def parseTagHandle() = {
        in.peekNext() match { // peeking next char!! current char is exclamation mark
          case ' ' =>
            in.skipCharacter() // skip exclamation mark
            TagHandle.Primary
          case '!' =>
            in.skipN(2) // skip both exclamation marks
            TagHandle.Secondary
          case _ =>
            val sb = new StringBuilder
            sb.append(in.read())
            def condition = {
              val c = in.peek()
              !c.isWhitespace && c != '!'
            }
            while (condition) { sb.append(in.read()) }
            sb.append(in.read())
            TagHandle.Named(sb.result())
        }
      }

      def parseTagPrefix() = {
        skipSpaces()
        in.peek() match {
          case '!' =>
            val sb = new StringBuilder
            while (!in.peek().isWhitespace) { sb.append(in.read()) }
            TagPrefix.Local(sb.result())
          case char if char != '!' && char != ',' =>
            val sb = new StringBuilder
            while (!in.peek().isWhitespace) { sb.append(in.read()) }
            TagPrefix.Global(sb.result())
          case _ => throw ScannerError.from(in.range, "Invalid tag prefix in TAG directive")
        }
      }

      skipSpaces()
      in.peek() match {
        case '!' =>
          val handle = parseTagHandle()
          val prefix = parseTagPrefix()
          List(Token(TokenKind.TagDirective(handle, prefix), range))
        case _ =>
          throw ScannerError.from(in.range, "Tag handle in TAG directive should start with '!'")
      }
    }

    in.peek() match {
      case 'Y' if in.peekN(4) == "YAML" =>
        in.skipN(4)
        parseYamlDirective()
      case 'T' if in.peekN(3) == "TAG" =>
        in.skipN(3)
        parseTagDirective()
      case _ => throw ScannerError.from(in.range, "Unknown directive, expected YAML or TAG")
    }
  }

  private def parseTag() = {
    val range        = in.range
    val invalidChars = Set('[', ']', '{', '}')

    def parseVerbatimTag(): String = {
      val sb = new StringBuilder
      sb.append('!')
      def condition = {
        val c = in.peek()
        c != '>' && !c.isWhitespace
      }
      while (condition) sb.append(in.read())
      in.peek() match {
        case '>' =>
          sb.append(in.read())
          sb.result()
        case _ => throw ScannerError.from(in.range, "Lacks '>' which closes verbatim tag attribute")
      }
    }

    def parseTagSuffix(): String = {
      val sb = new StringBuilder
      def condition = {
        val c = in.peek()
        !invalidChars(c) && !c.isWhitespace
      }
      while (condition) sb.append(in.read())

      if (invalidChars.contains(in.peek()))
        throw ScannerError.from(in.range, "Invalid character in tag")
      UrlDecoder.decode(sb.result())
    }

    def parseShorthandTag(second: Char): TagValue =
      second match {
        case '!' => // tag handle starts with '!!'
          in.skipCharacter()
          TagValue.Shorthand(TagHandle.Secondary, parseTagSuffix())
        case _ => // tag handle starts with '!<char>' where char isn't space
          val sb = new StringBuilder
          def condition = {
            val c = in.peek()
            !invalidChars(c) && !c.isWhitespace && c != '!'
          }
          while (condition)
            sb.append(in.read())
          if (invalidChars.contains(in.peek()))
            throw ScannerError.from(in.range, "Invalid character in tag")
          in.peek() match {
            case '!' =>
              sb.insert(0, '!')    // prepend already skipped exclamation mark
              sb.append(in.read()) // append ending exclamation mark
              TagValue.Shorthand(TagHandle.Named(sb.result()), parseTagSuffix())
            case ' ' =>
              TagValue.Shorthand(TagHandle.Primary, sb.result())
            case _ => throw ScannerError.from(in.range, "Invalid tag handle")
          }
      }

    in.skipCharacter() // skip first '!'
    val peeked = in.peek()
    val tag: Tag = peeked match {
      case Reader.nullTerminator =>
        throw ScannerError.from(in.range, "Input stream ended unexpectedly")
      case '<' =>
        val tag = parseVerbatimTag()
        Tag(TagValue.Verbatim(tag))
      case ' ' =>
        Tag(TagValue.NonSpecific)
      case char =>
        val tagValue = parseShorthandTag(char)
        Tag(tagValue)
    }

    if (ctx.isPlainKeyAllowed) {
      ctx.addPotentialKey(Token(tag, range))
      Nil
    } else List(Token(tag, range))
  }

  private def parseAnchorName(): (String, Range) = {
    val invalidChars = Set('[', ']', '{', '}', ',')
    val sb           = new StringBuilder

    @tailrec
    def readAnchorName(): String =
      in.peek() match {
        case Reader.nullTerminator => sb.result()
        case char if !invalidChars(char) && !in.isWhitespace =>
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
      ctx.addPotentialKey(anchorToken)
      Nil
    } else List(anchorToken)
  }

  private def parseAlias(): List[Token] = {
    val (name, pos) = parseAnchorName()
    val aliasToken  = Token(Alias(name), pos)
    if (ctx.isPlainKeyAllowed) {
      ctx.addPotentialKey(aliasToken)
      Nil
    } else List(aliasToken)
  }

  private def parseDoubleQuoteValue(): List[Token] = {
    val sb = new StringBuilder

    @tailrec
    def readScalar(): String =
      in.peek() match {
        case Reader.nullTerminator =>
          sb.result()
        case _ if in.isNewline =>
          skipUntilNextToken()
          sb.append(" ")
          readScalar()
        case '\\' if in.peekNext() == '"' =>
          in.skipN(2)
          sb.append("\"")
          readScalar()
        case '"' =>
          in.skipCharacter()
          sb.result()
        case char =>
          sb.append(in.read())
          readScalar()
      }

    val isPlainKeyAllowed = ctx.isPlainKeyAllowed
    val range             = in.range
    in.skipCharacter() // skip double quote
    val scalar      = readScalar()
    val endRange    = range.withEndPos(in.pos)
    val scalarToken = Token(Scalar(scalar, ScalarStyle.DoubleQuoted), endRange)
    if (isPlainKeyAllowed) {
      ctx.addPotentialKey(scalarToken)
      Nil
    } else List(scalarToken)
  }

  /**
   * This header is followed by a non-content line break with an optional comment.
   */
  private def parseBlockHeader(): Unit = {
    while (in.peek() == ' ')
      in.skipCharacter()

    if (in.peek() == '#')
      skipComment()

    if (in.isNewline) in.skipCharacter()
  }

  /**
   * final break interpretation - https://yaml.org/spec/1.2/#b-chomped-last(t)
   */
  private def parseChompingIndicator(): BlockChompingIndicator =
    in.peek() match {
      case '-' =>
        in.skipCharacter()
        BlockChompingIndicator.Strip
      case '+' =>
        in.skipCharacter()
        BlockChompingIndicator.Keep
      case _ => BlockChompingIndicator.Clip
    }

  private def parseIndentationIndicator(): Option[Int] =
    in.peek() match {
      case number if number.isDigit =>
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
        case Reader.nullTerminator => sb.result()
        case _ if in.isNewline =>
          ctx.isPlainKeyAllowed = true
          sb.append(in.read())
          skipUntilNextIndent(foldedIndent)
          if (!in.isWhitespace && in.column != foldedIndent) sb.result()
          else readLiteral()
        case char =>
          sb.append(in.read())
          readLiteral()
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
    def readFolded(
        prevCharWasNewline: Boolean = false,
        thisLineIsIndented: Boolean = false
    ): String = {
      in.peek() match {
        case Reader.nullTerminator => sb.result()
        case _ if in.isNewline =>
          ctx.isPlainKeyAllowed = true
          if (in.isNextNewline) {
            chompedEmptyLines()
            if (in.peek() != Reader.nullTerminator) {
              in.skipCharacter()
              skipUntilNextIndent(foldedIndent)
            }

            if (in.column != foldedIndent || in.peek() == Reader.nullTerminator) {
              if (chompingIndicator == BlockChompingIndicator.Keep) sb.append("\n")
              sb.result()
            } else {
              readFolded(prevCharWasNewline = true)
            }
          } else {
            in.skipCharacter() // skip newline

            skipUntilNextIndent(foldedIndent)

            if (in.column != foldedIndent || in.peek() == Reader.nullTerminator) {
              chompingIndicator match {
                case Keep => // if keep, strip all trailing newlines and spaces but count them and append counted amount of newlines
                  var count    = 1
                  var lastChar = sb.apply(sb.length - 1)
                  while (lastChar == '\n' || lastChar == ' ') {
                    sb.deleteCharAt(sb.length - 1)
                    lastChar = sb.apply(sb.length - 1)
                    count += 1
                  }
                  sb.append("\n" * count)
                case Strip => // if strip, strip all trailing newlines and spaces
                  var lastChar = sb.apply(sb.length - 1)
                  while (lastChar == '\n' || lastChar == ' ') {
                    sb.deleteCharAt(sb.length - 1)
                    lastChar = sb.apply(sb.length - 1)
                  }
                case Clip => // if clip, strip all trailing newlines and spaces and append a single newline
                  var lastChar = sb.apply(sb.length - 1)
                  while (lastChar == '\n' || lastChar == ' ') {
                    sb.deleteCharAt(sb.length - 1)
                    lastChar = sb.apply(sb.length - 1)
                  }
                  sb.append('\n')
              }

              sb.result() // final result
            } else {
              if (prevCharWasNewline || thisLineIsIndented) {
                sb.append("\n")
              } else {
                sb.append(" ")
              }

              readFolded(prevCharWasNewline = true)
            }
          }

        case ' ' if in.column == foldedIndent => // beginning of a line that is indented
          if (prevCharWasNewline) {      // we are at the beginning of a line that is indented
            sb.update(sb.size - 1, '\n') // replace last space with a newline
          }

          sb.append(in.read())
          readFolded(thisLineIsIndented = true)

        case char =>
          sb.append(in.read())
          readFolded(thisLineIsIndented = thisLineIsIndented)
      }
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
        case Reader.nullTerminator => sb.result()
        case '\'' if in.peekNext() == '\'' =>
          in.skipN(2)
          sb.append('\'')
          readScalar()
        case '\n' =>
          sb.append(' ')
          skipUntilNextToken()
          readScalar()
        case '\'' =>
          in.skipCharacter()
          sb.result()
        case char =>
          sb.append(in.read())
          readScalar()
      }

    val isPlainKeyAllowed = ctx.isPlainKeyAllowed
    val range             = in.range
    in.skipCharacter() // skip single quote
    val scalar      = readScalar()
    val endRange    = range.withEndPos(in.pos)
    val scalarToken = Token(Scalar(scalar, ScalarStyle.SingleQuoted), endRange)
    if (isPlainKeyAllowed) {
      ctx.addPotentialKey(scalarToken)
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
        case Reader.nullTerminator                                       => sb.result()
        case ':' if in.isNextWhitespace                                  => sb.result()
        case ':' if in.peekNext() == ',' && ctx.isInFlowCollection       => sb.result()
        case char if !ctx.isAllowedSpecialCharacter(char)                => sb.result()
        case _ if (isDocumentEnd || isDocumentStart) && ctx.indent == -1 => sb.result()
        case ' ' if in.peekNext() == '#'                                 => sb.result()
        case _ if in.isNewline =>
          ctx.isPlainKeyAllowed = true
          if (in.isNextNewline) chompedEmptyLines()
          else sb.append(' ')
          skipUntilNextToken()
          if (in.column > ctx.indent)
            readScalar()
          else sb.result()
        case char =>
          sb.append(in.read())
          readScalar()
      }
    }

    val isPlainKeyAllowed = ctx.isPlainKeyAllowed
    val range             = in.range
    val scalar            = readScalar()
    val endRange          = range.withEndPos(in.pos)
    val scalarToken       = Token(Scalar(scalar.trim, ScalarStyle.Plain), endRange)
    if (isPlainKeyAllowed) {
      ctx.addPotentialKey(scalarToken)
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
      if (ctx.isInBlockCollection && ctx.indent < firstSimpleKey.start.column) {
        ctx.addIndent(firstSimpleKey.start.column)
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
      throw ScannerError.from("Mapping value is not allowed", mappingValueToken)
    else
      maybeMappingStart ++ List(Token(MappingKey, in.range)) ++ potentialKeys :+ mappingValueToken
  }

  def skipUntilNextToken(): Unit = {
    while (in.isWhitespace && !in.isNewline) in.skipCharacter()

    if (in.peek() == '#') skipComment()

    if (in.isNewline) {
      ctx.isPlainKeyAllowed = true
      in.skipCharacter()
      skipUntilNextToken()
    }
  }

  def skipSpaces(): Unit =
    while (in.peek() == ' ') in.skipCharacter()

  def skipUntilNextIndent(indentBlock: Int): Unit =
    while (in.peek() == ' ' && in.column < indentBlock) in.skipCharacter()

  def skipUntilNextChar() =
    while (in.isWhitespace) in.skipCharacter()

  private def skipComment(): Unit = while (in.peek() != Reader.nullTerminator && !in.isNewline)
    in.skipCharacter()
}
