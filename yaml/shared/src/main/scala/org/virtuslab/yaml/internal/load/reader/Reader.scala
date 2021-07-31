package org.virtuslab.yaml.internal.load.reader

import scala.util.Try
import scala.annotation.tailrec

final case class Position(offset: Int, line: Int, column: Int)

trait Reader:
  def read(): Char
  def peek(n: Int = 0): Option[Char]
  def peekNext(): Option[Char]
  def peekN(n: Int): String
  def isNextWhitespace: Boolean
  def isNextNewline: Boolean
  def skipCharacter(): Unit
  def skipN(n: Int): Unit

  def line: Int
  def column: Int
  def offset: Int
  def pos(): Position

private[yaml] class StringReader(in: CharSequence) extends Reader:
  var line: Int   = 1
  var column: Int = 1
  var offset: Int = 0

  override def pos() = Position(offset, line, column)

  override def peek(n: Int = 0): Option[Char] =
    if offset + n < in.length then Some(in.charAt(offset + n))
    else None

  override def peekNext(): Option[Char] = peek(1)
  override def peekN(n: Int): String    = (0 until n).map(peek(_)).flatten.mkString("")
  override def isNextWhitespace         = peekNext().exists(_.isWhitespace)
  override def isNextNewline            = peekNext().exists(c => c == '\n' || c == '\r')

  private inline def nextLine() = { column = 1; line += 1 }
  private def skipAndMantainPosition() =
    val next = in.charAt(offset)
    if next == '\r' then
      offset += 2
      nextLine()
      2
    else if next == '\n' then
      offset += 1
      nextLine()
      1
    else
      offset += 1
      column += 1
      1

  override def skipN(n: Int): Unit =
    @tailrec def loop(left: Int): Unit =
      if left <= 0 then ()
      else
        val skipped = skipAndMantainPosition()
        loop(left - skipped)
    loop(n)

  override def skipCharacter(): Unit = skipAndMantainPosition()

  override def read(): Char =
    skipCharacter()
    in.charAt(offset - 1)

end StringReader
