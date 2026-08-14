package org.virtuslab.yaml.internal.load.reader

import org.virtuslab.yaml.Position
import org.virtuslab.yaml.Range

trait Reader {

  /** Read current character and advance by 1 position
    * @return current character
    */
  def read(): Char

  /** Read current character without advancing
    * @return current character or '\u0000' in case there are no chars left
    */
  def peek(n: Int = 0): Char

  def line: Int
  def column: Int
  def offset: Int
  def pos: Position
  def range: Range

  def skipCharacter(): Unit
  def skipN(n: Int): Unit
  def skipWhitespaces(): Unit
  def peekN(n: Int): String

  final def peekNext(): Char          = peek(1)
  final def isWhitespace: Boolean     = Character.isWhitespace(peek(0))
  final def isNextWhitespace: Boolean = Character.isWhitespace(peek(1))
  final def isNewline: Boolean        = isNewlineN(0)
  final def isNextNewline: Boolean    = isNewlineN(1)

  private def isNewlineN(n: Int): Boolean = {
    val c = peek(n)
    c == '\n' || isWindowsNewline(c)
  }
  protected def isWindowsNewline(c: Char): Boolean = c == '\r' && peek(1) == '\n'
}

object Reader {
  final val nullTerminator: Char = '\u0000'
}

private[yaml] class StringReader(in: String) extends Reader {
  private val len = in.length
  var line: Int   = 0
  var column: Int = 0
  var offset: Int = 0
  val lines       = in.split("\n", -1).toVector

  override def pos = new Position(offset, line, column)

  override def range = new Range(pos, lines)

  override def peek(n: Int = 0): Char = {
    val i = offset + n
    if (i < len) in.charAt(i)
    else '\u0000'
  }

  override def peekN(n: Int): String = {
    val end = offset + n
    if (end <= len) in.substring(offset, end)
    else {
      val available = Math.max(len - offset, 0)
      val padding   = new String(new Array[Char](n - available))
      if (available > 0) in.substring(offset, len).concat(padding)
      else padding
    }
  }

  override def skipN(n: Int): Unit = {
    val limit = offset + n
    while (offset < limit) skipCharacter()
  }

  override def skipCharacter(): Unit = {
    var i = offset
    var c = in.charAt(i)
    i += 1
    if (
      c == '\n' || c == '\r' && {
        i < len && {
          c = in.charAt(i)
          i += 1
          c == '\n'
        }
      }
    ) {
      column = 0
      line += 1
    } else column += 1
    offset = i
  }

  override def skipWhitespaces(): Unit = {
    var i       = offset
    var c: Char = 0
    while ({
      i < len && {
        c = in.charAt(i)
        Character.isWhitespace(c)
      }
    }) {
      i += 1
      if (
        c == '\n' || c == '\r' && {
          i < len && {
            c = in.charAt(i)
            i += 1
            c == '\n'
          }
        }
      ) {
        column = 0
        line += 1
      } else column += 1
    }
    offset = i
  }

  override def read(): Char = {
    var i = offset
    var c = in.charAt(i)
    i += 1
    if (
      c == '\n' || c == '\r' && {
        i < len && {
          c = in.charAt(i)
          i += 1
          c == '\n'
        }
      }
    ) {
      column = 0
      line += 1
    } else column += 1
    offset = i
    c
  }
}
