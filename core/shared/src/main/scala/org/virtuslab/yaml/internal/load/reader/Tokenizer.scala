package org.virtuslab.yaml.internal.load.reader

import scala.annotation.{switch, tailrec}
import org.virtuslab.yaml.Range
import org.virtuslab.yaml.ScannerError
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.internal.load.TagHandle
import org.virtuslab.yaml.internal.load.TagPrefix
import org.virtuslab.yaml.internal.load.TagValue
import org.virtuslab.yaml.internal.load.reader.token.BlockChompingIndicator
import org.virtuslab.yaml.internal.load.reader.token.BlockChompingIndicator.*
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind
import org.virtuslab.yaml.internal.load.reader.token.TokenKind.*

import scala.collection.mutable

trait Tokenizer {
  def peekToken(): Either[YamlError, Token]

  /**
   * Peek a token or throw a ScannerError
   * @return a token
   */
  def peekTokenUnsafe(): Token
  def popToken(): Token
}

object Tokenizer {
  def make(str: String): Tokenizer = new StringTokenizer(str)
}

private final class StringTokenizer(str: String) extends Tokenizer {

  private val ctx = TokenizerContext(str)
  private val in  = ctx.reader

  override def peekToken(): Either[YamlError, Token] = {
    val tokens = ctx.tokens
    while (tokens.isEmpty) {
      try appendNextTokens(tokens)
      catch {
        case e: ScannerError => return new Left(e)
      }
    }
    new Right(tokens.apply(0))
  }

  override def peekTokenUnsafe(): Token = {
    val tokens = ctx.tokens
    while (tokens.isEmpty) appendNextTokens(tokens)
    tokens.apply(0)
  }

  override def popToken(): Token = ctx.tokens.removeHead()

  /**
  * Plain keys have to be resolved in the same line they were created, otherwise they are ordinary tokens.
  */
  private def shouldPopPlainKeys: Boolean =
    ctx.isInBlockCollection && ctx.potentialKeyOpt
      .exists(_.range.start.line != in.line)

  private def appendNextTokens(queue: mutable.ArrayDeque[Token]): Unit = {
    skipUntilNextToken()
    val closedBlockTokens = ctx.checkIndents(in.column)
    if (closedBlockTokens.nonEmpty || shouldPopPlainKeys) queue.appendAll(ctx.popPotentialKeys())
    queue.appendAll(closedBlockTokens)
    (in.peek(): @switch) match {
      case '[' =>
        in.skipCharacter()
        ctx.enterFlowSequence
        queue.appendAll(ctx.popPotentialKeys()).append(new Token(FlowSequenceStart, in.range))
      case ']' =>
        in.skipCharacter()
        ctx.leaveFlowSequence
        queue.appendAll(ctx.popPotentialKeys()).append(new Token(FlowSequenceEnd, in.range))
      case '{' =>
        in.skipCharacter()
        ctx.enterFlowMapping
        ctx.isPlainKeyAllowed = true
        queue.appendAll(ctx.popPotentialKeys()).append(new Token(FlowMappingStart, in.range))
      case '}' =>
        in.skipCharacter()
        ctx.leaveFlowMapping
        queue.appendAll(ctx.popPotentialKeys()).append(new Token(FlowMappingEnd, in.range))
      case '&' =>
        val anchorToken = parseAnchorToken(false)
        if (ctx.isPlainKeyAllowed) ctx.addPotentialKey(anchorToken)
        else queue.append(anchorToken)
      case '!' =>
        val range = in.range

        def parseVerbatimTag(): String = {
          val sb = new java.lang.StringBuilder
          sb.append('!')
          while ({
            val c = in.peek()
            c != '>' && !Character.isWhitespace(c)
          }) sb.append(in.read())
          if (in.peek() != '>') {
            throw ScannerError.from(in.range, "Lacks '>' which closes verbatim tag attribute")
          }
          sb.append(in.read()).toString
        }

        def parseTagSuffix(): String = {
          val sb = new java.lang.StringBuilder
          while ({
            val c = in.peek()
            if (c == '[' || c == ']' || c == '{' || c == '}') {
              throw ScannerError.from(in.range, "Invalid character in tag")
            } else if (c == '\u0000') {
              throw ScannerError.from(in.range, "Input stream ended unexpectedly")
            }
            !Character.isWhitespace(c)
          }) sb.append(in.read())
          try UrlDecoder.decode(sb.toString)
          catch {
            case _: IllegalArgumentException =>
              throw ScannerError.from(in.range, "Invalid percent-encoding in tag")
          }
        }

        def parseShorthandTag(second: Char): TagValue =
          second match {
            case '!' => // tag handle starts with '!!'
              in.skipCharacter()
              new TagValue.Shorthand(TagHandle.Secondary, parseTagSuffix())
            case _ => // tag handle starts with '!<char>' where char isn't space
              val sb = new java.lang.StringBuilder
              while ({
                val c = in.peek()
                if (c == '[' || c == ']' || c == '{' || c == '}') {
                  throw ScannerError.from(in.range, "Invalid character in tag")
                } else if (c == '\u0000') {
                  throw ScannerError.from(in.range, "Input stream ended unexpectedly")
                }
                !Character.isWhitespace(c) && c != '!'
              }) sb.append(in.read())
              in.peek() match {
                case '!' =>
                  sb.insert(0, '!')    // prepend already skipped exclamation mark
                  sb.append(in.read()) // append ending exclamation mark
                  new TagValue.Shorthand(new TagHandle.Named(sb.toString), parseTagSuffix())
                case ' ' =>
                  new TagValue.Shorthand(TagHandle.Primary, sb.toString)
                case _ => throw ScannerError.from(in.range, "Invalid tag handle")
              }
          }

        in.skipCharacter() // skip first '!'
        val tag = new Tag(in.peek() match {
          case '<' =>
            new TagValue.Verbatim(parseVerbatimTag())
          case ' ' =>
            TagValue.NonSpecific
          case c =>
            if (c == '\u0000') {
              throw ScannerError.from(in.range, "Input stream ended unexpectedly")
            }
            parseShorthandTag(c)
        })
        val token = new Token(tag, range)
        if (ctx.isPlainKeyAllowed) ctx.addPotentialKey(token)
        else queue.append(token)
      case '%' =>
        val range = in.range
        in.skipCharacter() // skip %
        in.peek() match {
          case 'Y' if in.peekN(4) == "YAML" =>
            in.skipN(4)
            throw ScannerError.from(in.range, "YAML directives are not supported yet.")
          case 'T' if in.peekN(3) == "TAG" =>
            in.skipN(3)

            def parseTagHandle(): TagHandle =
              in.peek(1) match { // peeking next char!! current char is exclamation mark
                case ' ' =>
                  in.skipCharacter() // skip exclamation mark
                  TagHandle.Primary
                case '!' =>
                  in.skipN(2) // skip both exclamation marks
                  TagHandle.Secondary
                case _ =>
                  val sb = new java.lang.StringBuilder
                  sb.append(in.read())
                  while ({
                    val c = in.peek()
                    !Character.isWhitespace(c) && c != '!'
                  }) sb.append(in.read())
                  sb.append(in.read())
                  new TagHandle.Named(sb.toString)
              }

            def parseTagPrefix(): TagPrefix = {
              skipSpaces()
              val c = in.peek()
              if (c == ',') {
                throw ScannerError.from(in.range, "Invalid tag prefix in TAG directive")
              }
              val sb = new java.lang.StringBuilder
              while (!Character.isWhitespace(in.peek())) sb.append(in.read())
              val prefix = sb.toString
              if (c == '!') new TagPrefix.Local(prefix)
              else new TagPrefix.Global(prefix)
            }

            skipSpaces()
            if (in.peek() != '!') {
              throw ScannerError.from(in.range, "Tag handle in TAG directive should start with '!'")
            }
            queue.append(
              new Token(new TokenKind.TagDirective(parseTagHandle(), parseTagPrefix()), range)
            )
          case _ => throw ScannerError.from(in.range, "Unknown directive, expected YAML or TAG")
        }
      case '"' =>
        val sb = new java.lang.StringBuilder

        @tailrec
        def readScalar(): String = in.peek() match {
          case '"' =>
            in.skipCharacter()
            sb.toString
          case '\\' if in.peek(1) == '"' =>
            in.skipN(2)
            sb.append('"')
            readScalar()
          case '\u0000' =>
            sb.toString
          case c =>
            if (c == '\n' || c == '\r' && in.peek(1) == '\n') {
              skipUntilNextToken()
              sb.append(' ')
              readScalar()
            } else {
              in.skipCharacter()
              sb.append(c)
              readScalar()
            }
        }

        val isPlainKeyAllowed = ctx.isPlainKeyAllowed
        val range             = in.range
        in.skipCharacter() // skip double quote
        val scalarToken =
          new Token(Scalar(readScalar(), ScalarStyle.DoubleQuoted), range.withEndPos(in.pos))
        if (isPlainKeyAllowed) ctx.addPotentialKey(scalarToken)
        else queue.append(scalarToken)
      case '\'' =>
        val sb = new java.lang.StringBuilder

        @tailrec
        def readScalar(): String = (in.peek(): @switch) match {
          case '\'' =>
            if (in.peek(1) == '\'') {
              in.skipN(2)
              sb.append('\'')
              readScalar()
            } else {
              in.skipCharacter()
              sb.toString
            }
          case '\n' =>
            sb.append(' ')
            skipUntilNextToken()
            readScalar()
          case '\u0000' => sb.toString
          case c =>
            in.skipCharacter()
            sb.append(c)
            readScalar()
        }

        val isPlainKeyAllowed = ctx.isPlainKeyAllowed
        val range             = in.range
        in.skipCharacter() // skip single quote
        val scalarToken =
          new Token(Scalar(readScalar(), ScalarStyle.SingleQuoted), range.withEndPos(in.pos))
        if (isPlainKeyAllowed) ctx.addPotentialKey(scalarToken)
        else queue.append(scalarToken)
      case '>' =>
        val sb    = new java.lang.StringBuilder
        val range = in.range
        in.skipCharacter() // skip >
        var indentation       = parseIndentationIndicator()
        val chompingIndicator = parseChompingIndicator()
        if (indentation eq None) indentation = parseIndentationIndicator()
        parseBlockHeader()
        if (indentation eq None) skipUntilNextToken()
        val foldedIndent = indentation match {
          case Some(fi) => fi
          case _        => in.column
        }
        skipUntilNextIndent(foldedIndent)

        @tailrec
        def readFolded(
            prevCharWasNewline: Boolean = false,
            thisLineIsIndented: Boolean = false
        ): String = {
          in.peek() match {
            case _ if in.isNewline =>
              ctx.isPlainKeyAllowed = true
              if (in.isNextNewline) {
                while (in.isNextNewline) {
                  in.skipCharacter()
                  sb.append('\n')
                }
                if (in.peek() != '\u0000') {
                  in.skipCharacter()
                  skipUntilNextIndent(foldedIndent)
                }
                if (in.column != foldedIndent || in.peek() == '\u0000') {
                  if (chompingIndicator eq BlockChompingIndicator.Keep) sb.append('\n')
                  sb.toString
                } else readFolded(prevCharWasNewline = true)
              } else {
                in.skipCharacter() // skip newline
                skipUntilNextIndent(foldedIndent)
                if (in.column != foldedIndent || in.peek() == '\u0000') {
                  chompingIndicator match {
                    case _: Keep.type => // if keep, strip all trailing newlines and spaces but count them and append counted amount of newlines
                      var count = 1
                      while ({
                        val lastChar = sb.charAt(sb.length - 1)
                        lastChar == '\n' || lastChar == ' '
                      }) {
                        sb.deleteCharAt(sb.length - 1)
                        count += 1
                      }
                      while (count > 0) {
                        sb.append('\n')
                        count -= 1
                      }
                    case _: Strip.type => // if strip, strip all trailing newlines and spaces
                      while ({
                        val lastChar = sb.charAt(sb.length - 1)
                        lastChar == '\n' || lastChar == ' '
                      }) sb.deleteCharAt(sb.length - 1)
                    case _ => // if clip, strip all trailing newlines and spaces and append a single newline
                      while ({
                        val lastChar = sb.charAt(sb.length - 1)
                        lastChar == '\n' || lastChar == ' '
                      }) sb.deleteCharAt(sb.length - 1)
                      sb.append('\n')
                  }
                  sb.toString // final result
                } else {
                  sb.append({
                    if (prevCharWasNewline || thisLineIsIndented) '\n'
                    else ' '
                  })
                  readFolded(prevCharWasNewline = true)
                }
              }
            case ' ' if in.column == foldedIndent => // beginning of a line that is indented
              if (prevCharWasNewline) { // we are at the beginning of a line that is indented
                sb.setCharAt(sb.length() - 1, '\n') // replace last space with a newline
              }
              sb.append(in.read())
              readFolded(thisLineIsIndented = true)
            case '\u0000' => sb.toString
            case _ =>
              sb.append(in.read())
              readFolded(thisLineIsIndented = thisLineIsIndented)
          }
        }

        val chompedScalar = chompingIndicator.removeBlankLinesAtEnd(readFolded())
        queue.append(new Token(Scalar(chompedScalar, ScalarStyle.Folded), range))
      case '|' =>
        val sb    = new java.lang.StringBuilder
        val range = in.range
        in.skipCharacter() // skip |
        var indentation       = parseIndentationIndicator()
        val chompingIndicator = parseChompingIndicator()
        if (indentation eq None) indentation = parseIndentationIndicator()
        parseBlockHeader()
        if (indentation eq None) in.skipWhitespaces()
        val foldedIndent = indentation match {
          case Some(fi) => fi
          case _        => in.column
        }
        skipUntilNextIndent(foldedIndent)

        @tailrec
        def readLiteral(): String =
          if (in.peek() == '\u0000') sb.toString
          else if (in.isNewline) {
            sb.append(in.read())
            ctx.isPlainKeyAllowed = true
            skipUntilNextIndent(foldedIndent)
            if (!in.isWhitespace && in.column != foldedIndent) sb.toString
            else readLiteral()
          } else {
            sb.append(in.read())
            readLiteral()
          }

        val chompedScalar = chompingIndicator.removeBlankLinesAtEnd(readLiteral())
        queue.append(new Token(Scalar(chompedScalar, ScalarStyle.Literal), range))
      case '*' =>
        val aliasToken = parseAnchorToken(true)
        if (ctx.isPlainKeyAllowed) ctx.addPotentialKey(aliasToken)
        else queue.append(aliasToken)
      case ',' =>
        in.skipCharacter()
        ctx.isPlainKeyAllowed = true
        queue.appendAll(ctx.popPotentialKeys()).append(new Token(Comma, in.range))
      case '\u0000' =>
        queue
          .appendAll(ctx.popPotentialKeys())
          .appendAll(ctx.checkIndents(-1))
          .append(new Token(StreamEnd, in.range))
      case c =>
        if (
          c == ':' && (in.isNextWhitespace || (ctx.isInFlowCollection && ctx.isPlainKeyAllowed))
        ) {
          in.skipCharacter() // skip
          val mappingValueToken = new Token(MappingValue, in.range)
          lazy val firstSimpleKey = ctx.potentialKeys.headOption.getOrElse(
            throw ScannerError.from("Not found expected key for value", mappingValueToken)
          )
          if (ctx.isInBlockCollection && ctx.indent < firstSimpleKey.start.column) {
            ctx.addIndent(firstSimpleKey.start.column)
            queue.append(new Token(MappingStart, firstSimpleKey.range))
          }
          val potentialKeys = ctx.popPotentialKeys()
          ctx.isPlainKeyAllowed = false
          if (
            ctx.isInBlockCollection &&
            firstSimpleKey.range.end.exists(_.line > firstSimpleKey.range.start.line)
          ) throw ScannerError.from("Mapping value is not allowed", mappingValueToken)
          queue
            .append(new Token(MappingKey, in.range))
            .appendAll(potentialKeys)
            .append(mappingValueToken)
        } else if (c == '-' && isDocumentStart) {
          in.skipN(if (in.peek(3) == '\u0000') 3 else 4)
          queue.appendAll(ctx.parseDocumentStart(in.column))
        } else if (c == '-' && in.isNextWhitespace) {
          // when last indent is lesser than current one, it means that this is start of the sequence
          if (ctx.isInBlockCollection && ctx.indent < in.column) {
            ctx.addIndent(in.column)
            queue.append(new Token(SequenceStart, in.range))
          }
          if (ctx.isInBlockCollection && !ctx.isPlainKeyAllowed) {
            throw ScannerError.from(in.range, "cannot start sequence")
          }
          in.skipCharacter() // skip '-'
          queue.appendAll(ctx.popPotentialKeys()).append(new Token(SequenceValue, in.range))
        } else if (c == '.' && isDocumentEnd) {
          in.skipN(if (in.peek(3) == '\u0000') 3 else 4)
          queue.appendAll(ctx.parseDocumentEnd())
        } else {
          val sb = new java.lang.StringBuilder

          @tailrec
          def readScalar(): String = {
            val c = in.peek()
            if (
              c == '\u0000' ||
              c == ':' && (in.isNextWhitespace || in.peek(1) == ',' && ctx.isInFlowCollection) ||
              c == ' ' && in.peek(1) == '#' ||
              c == '.' && ctx.indent == -1 && isDocumentEnd ||
              c == '-' && ctx.indent == -1 && isDocumentStart ||
              !ctx.isAllowedSpecialCharacter(c)
            ) sb.toString
            else if (c == '\n' || c == '\r' && in.peek(1) == '\n') {
              ctx.isPlainKeyAllowed = true
              if (in.isNextNewline) {
                while (in.isNextNewline) {
                  in.skipCharacter()
                  sb.append('\n')
                }
              } else sb.append(' ')
              skipUntilNextToken()
              if (in.column > ctx.indent) readScalar()
              else sb.toString
            } else {
              in.skipCharacter()
              sb.append(c)
              readScalar()
            }
          }

          val isPlainKeyAllowed = ctx.isPlainKeyAllowed
          val range             = in.range
          val scalarToken =
            new Token(Scalar(readScalar().trim, ScalarStyle.Plain), range.withEndPos(in.pos))
          if (isPlainKeyAllowed) ctx.addPotentialKey(scalarToken)
          else queue.append(scalarToken)
        }
    }
  }

  private def isDocumentStart = {
    val c1 = in.peek(1)
    val c2 = in.peek(2)
    val c3 = in.peek(3)
    c1 == '-' && c2 == '-' && (Character.isWhitespace(c3) || c3 == '\u0000')
  }

  private def isDocumentEnd = {
    val c1 = in.peek(1)
    val c2 = in.peek(2)
    val c3 = in.peek(3)
    c1 == '.' && c2 == '.' && (Character.isWhitespace(c3) || c3 == '\u0000')
  }

  private def parseAnchorToken(isAlias: Boolean): Token = {
    val sb = new java.lang.StringBuilder

    @tailrec
    def readAnchorName(): String = {
      val c = in.peek()
      if (c == '\u0000') sb.toString
      else if (!(c == '[' || c == ']' || c == '{' || c == '}' || c == ',') && !in.isWhitespace) {
        sb.append(in.read())
        readAnchorName()
      } else sb.toString
    }

    val range = in.range
    in.skipCharacter()
    val name = readAnchorName()
    new Token(
      {
        if (isAlias) new Alias(name)
        else new Anchor(name)
      },
      range
    )
  }

  /**
   * This header is followed by a non-content line break with an optional comment.
   */
  private def parseBlockHeader(): Unit = {
    var c: Char = 0
    while ({
      c = in.peek()
      c == ' '
    }) in.skipCharacter()
    if (c == '#') skipComment()
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

  private def parseIndentationIndicator(): Option[Int] = {
    val c = in.peek()
    if (c.isDigit) {
      in.skipCharacter()
      new Some(c.asDigit)
    } else None
  }

  def skipUntilNextToken(): Unit = {
    var c: Char = 0
    while ({
      c = in.peek()
      if (c == '#') {
        skipComment()
        c = in.peek()
      }
      Character.isWhitespace(c)
    }) {
      if (c == '\n' || c == '\r' && in.peek(1) == '\n') ctx.isPlainKeyAllowed = true
      in.skipCharacter()
    }
  }

  def skipSpaces(): Unit =
    while (in.peek() == ' ') in.skipCharacter()

  def skipUntilNextIndent(indentBlock: Int): Unit =
    while (in.peek() == ' ' && in.column < indentBlock) in.skipCharacter()

  def skipUntilNextChar() = in.skipWhitespaces()

  private def skipComment(): Unit =
    while (in.peek() != '\u0000' && !in.isNewline) in.skipCharacter()
}
