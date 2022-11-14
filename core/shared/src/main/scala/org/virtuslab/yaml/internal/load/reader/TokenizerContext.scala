package org.virtuslab.yaml.internal.load.reader

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayDeque

import org.virtuslab.yaml.Range
import org.virtuslab.yaml.internal.load.reader.Reader
import org.virtuslab.yaml.internal.load.reader.StringReader
import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind._

private[reader] case class TokenizerContext(reader: Reader) {
  val tokens = mutable.ArrayDeque.empty[Token]

  var isPlainKeyAllowed: Boolean = true
  private val indentations       = mutable.ArrayDeque.empty[Int]
  private var flowSequenceLevel  = 0
  private var flowMappingLevel   = 0

  def indent: Int                     = indentations.lastOption.getOrElse(-1)
  def addIndent(newIndent: Int): Unit = indentations.append(newIndent)
  def removeLastIndent(): Unit        = if (indentations.nonEmpty) indentations.removeLast()

  /**
    * Stores tokens which might be assosiated with simple key (scalar). Such key might start with
    * - tag
    * - anchor
    * - alias
    * - scalar
    */
  val potentialKeys                     = mutable.ArrayDeque.empty[Token]
  def addPotentialKey(key: Token): Unit = potentialKeys.addOne(key)
  def popPotentialKeys(): List[Token]   = potentialKeys.removeAll().toList
  def potentialKeyOpt: Option[Token]    = potentialKeys.headOption

  def needMoreTokens(): Boolean =
    tokens.isEmpty || potentialKeys.nonEmpty

  def checkIndents(current: Int): List[Token] =
    if (current < indent) {
      indentations.removeLast()
      Token(BlockEnd, reader.range) +: checkIndents(current)
    } else Nil

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

  def isInBlockCollection: Boolean = !isInFlowCollection

  def parseDocumentStart(indent: Int): List[Token] =
    checkIndents(-1) ++ List(Token(DocumentStart, reader.range))

  def parseDocumentEnd(): List[Token] =
    popPotentialKeys() ++ checkIndents(-1) ++ List(Token(DocumentEnd, reader.range))
}

private[reader] object TokenizerContext {
  def apply(in: String): TokenizerContext = TokenizerContext(new StringReader(in))
}
