package org.virtuslab.yaml.internal.load.reader

import org.virtuslab.yaml.Position
import org.virtuslab.yaml.internal.load.reader.Reader
import org.virtuslab.yaml.internal.load.reader.StringReader

import scala.annotation.tailrec
import scala.collection.mutable
import token.Token
import token.TokenKind.*
import scala.collection.mutable.ArrayDeque

case class ReaderCtx(reader: Reader) {
  val tokens                    = mutable.ArrayDeque.empty[Token]
  private val indentations      = mutable.ArrayDeque.empty[Int]
  private var flowSequenceLevel = 0
  private var flowMappingLevel  = 0

  def indent: Int                     = indentations.lastOption.getOrElse(-1)
  def addIndent(newIndent: Int): Unit = indentations.append(newIndent)
  def removeLastIndent(): Unit        = if (indentations.nonEmpty) indentations.removeLast()

  def checkIndents(current: Int): List[Token] =
    if current < indent then
      indentations.removeLast()
      Token(BlockEnd, reader.pos) +: checkIndents(current)
    else Nil

  def enterFlowSequence: Unit = flowSequenceLevel += 1
  def leaveFlowSequence: Unit = flowSequenceLevel -= 1

  def enterFlowMapping: Unit = flowMappingLevel += 1
  def leaveFlowMapping: Unit = flowMappingLevel -= 1

  def isAllowedSpecialCharacter(char: Char): Boolean =
    if ((char == ',' || char == '}') && flowMappingLevel > 0) false
    else if ((char == ',' || char == ']') && flowSequenceLevel > 0) false
    else true

  def isInFlowMapping: Boolean    = flowMappingLevel > 0
  def isInFlowSequence: Boolean   = flowSequenceLevel > 0
  def isInFlowCollection: Boolean = isInFlowMapping || isInFlowSequence

  def parseDocumentStart(indent: Int): List[Token] =
    checkIndents(-1) ++ List(Token(DocumentStart, reader.pos))

  def parseDocumentEnd(): List[Token] =
    checkIndents(-1) ++ List(Token(DocumentEnd, reader.pos))
}

object ReaderCtx:
  def apply(in: String): ReaderCtx = ReaderCtx(StringReader(in))
