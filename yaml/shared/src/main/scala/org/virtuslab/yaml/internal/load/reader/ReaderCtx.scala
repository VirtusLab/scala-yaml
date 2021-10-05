package org.virtuslab.yaml.internal.load.reader

import org.virtuslab.yaml.Position
import org.virtuslab.yaml.internal.load.reader.Reader
import org.virtuslab.yaml.internal.load.reader.StringReader

import scala.annotation.tailrec
import scala.collection.mutable
import token.Token
import scala.collection.mutable.ArrayDeque

case class ReaderCtx(
    stateStack: mutable.Stack[ReaderState],
    tokens: mutable.ArrayDeque[Token] = mutable.ArrayDeque.empty,
    reader: Reader
) {

  private val indentations = mutable.ArrayDeque.empty[Int]
  private var flowSequenceLevel = 0
  private var flowMappingLevel = 0

  def indent: Int = indentations.lastOption.getOrElse(-1)
  def addIndent(newIndent: Int): Unit = indentations.append(newIndent)

  def checkIndents(current: Int): Unit = 
    if current < indent then
      indentations.removeLast()
      tokens.append(Token.BlockEnd(reader.pos()))
      checkIndents(current)
    else ()
    
  def enterFlowSequence: Unit = flowSequenceLevel += 1
  def leaveFlowSequence: Unit = flowSequenceLevel -= 1

  def enterFlowMapping: Unit = flowMappingLevel += 1
  def leaveFlowMapping: Unit = flowMappingLevel -= 1

  def isAllowedSpecialCharacter(char: Char): Boolean =
    if ((char == ',' || char == '}') && flowMappingLevel > 0) false
    else if ((char == ',' || char == ']') && flowSequenceLevel > 0) false
    else true

  def isFlowMapping(): Boolean = flowMappingLevel > 0

  def parseDocumentStart(indent: Int): List[Token] =
    checkIndents(-1)
    List(Token.DocumentStart(reader.pos()))

  def parseDocumentEnd(): List[Token] =
    checkIndents(-1)
    List(Token.DocumentEnd(reader.pos()))
}

case object ReaderCtx:
  def init(in: String): ReaderCtx =
    ReaderCtx(mutable.Stack.empty, mutable.ArrayDeque.empty, StringReader(in))
