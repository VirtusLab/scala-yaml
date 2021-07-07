package org.virtuslab.internal.load

import scala.concurrent.duration.Duration

trait YamlReader:
  def peek: Char
  def read(): Char
  def rewind(): Unit
  def skip(): Unit = read()
  def hasNext: Boolean
  def hasCurrent: Boolean

class StringYamlReader(in: CharSequence) extends YamlReader:
  private var offset = 0

  override def hasNext: Boolean = offset + 1 < in.length()
  override def hasCurrent: Boolean = offset < in.length()
  override def peek: Char = in.charAt(offset)
  override def rewind(): Unit = offset -= 1

  override def read(): Char =
    offset += 1
    in.charAt(offset - 1)
