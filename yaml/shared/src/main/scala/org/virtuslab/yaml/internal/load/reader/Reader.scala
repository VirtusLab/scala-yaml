package org.virtuslab.yaml.internal.load.reader

import scala.util.Try

trait Reader:
  def read(): Char
  def peek(n: Int = 0): Option[Char]
  def peekNext(): Option[Char]
  def peekN(n: Int): String
  def isNextWhitespace: Boolean
  def skipCharacter(): Unit
  def skipN(n: Int): Unit
  def revert(n: Int): Unit

  def line: Int
  def column: Int
  def offset: Int

private[yaml] class StringReader(in: CharSequence) extends Reader:
  var line: Int   = 0
  var column: Int = 0
  var offset: Int = 0

  override def peek(n: Int = 0): Option[Char] =
    if offset + n < in.length then Some(in.charAt(offset + n))
    else None

  override def peekNext(): Option[Char] = peek(1)
  override def peekN(n: Int): String    = (0 until n).map(peek(_)).flatten.mkString("")
  override def isNextWhitespace         = peekNext().exists(_.isWhitespace)

  override def revert(n: Int): Unit  = offset -= n
  override def skipN(n: Int): Unit   = offset += n
  override def skipCharacter(): Unit = skipN(1)

  override def read(): Char =
    skipCharacter()
    in.charAt(offset - 1)

end StringReader
