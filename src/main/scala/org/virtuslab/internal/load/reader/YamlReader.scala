package org.virtuslab.internal.load.reader

import org.virtuslab.internal.load.reader
import token.Token

import sun.security.util.Length

import scala.util.Try

trait Reader:
  def getToken(): Token
  def skipUntilNextToken(): Unit

class YamlReader(in: CharSequence) extends Reader {

  val ctx: ReaderCtx      = ReaderCtx.init
  private var indent: Int = 0
  private var offset      = 0

  def getToken(): Token =

    skipUntilNextToken()
    peek() match
      case Some('-') => {
        if (ctx.shouldParseSequenceNode(indent)) then
          read()
          indent += 1
          fetchValue()
        else
          val token = ctx.appendSequence(indent)
          token
      }
      case Some('[') =>
        read()
        ctx.appendSequence(indent)
      case Some(']') =>
        read()
        ctx.popTokenFromStack
      case Some(_) => fetchValue()
      case None    => ctx.popTokenFromStack

  private def getScalar(): String = {

    val sb = new StringBuilder

    def readScalar(): String =
      peek() match
        case Some(':') if peekNext() == Some(' ') || peekNext() == Some('\n') => sb.result()
        case Some('\n') | Some('#') | Some(']') | None                        => sb.result()
        case Some(',') =>
          read()
          sb.result()
        case Some(char) =>
          sb.append(read())
          readScalar()

    readScalar()
  }

  private def fetchValue(): Token = {

    skipUntilNextToken()
    val index = offset
    val value = getScalar()

    peek() match {
      case Some(':') => {
        if (ctx.shouldParseMappingNode(indent)) {
          read()
          Token.Scalar.from(value)
        } else {
          val token = ctx.appendMapping(indent)
          offset = index
          token
        }
      }
      case _ => Token.Scalar.from(value)
    }
  }

  def peek(): Option[Char]     = Try(in.charAt(offset)).toOption
  def peekNext(): Option[Char] = Try(in.charAt(offset + 1)).toOption

  def read(): Char = {
    offset += 1
    in.charAt(offset - 1)
  }

  def skipUntilNextToken(): Unit =
    while (peek() == Some(' ')) {
      indent += 1
      read()
    }

    if peek() == Some('\n') then {
      read()
      indent = 0
      skipUntilNextToken()
    }

}
