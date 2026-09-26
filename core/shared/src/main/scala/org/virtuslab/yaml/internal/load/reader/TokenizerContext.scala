package org.virtuslab.yaml.internal.load.reader

import scala.collection.mutable

import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind._

private[reader] case class TokenizerContext(reader: Reader) {
  val tokens = mutable.ArrayDeque.empty[Token]

  var isPlainKeyAllowed: Boolean      = true
  private[this] var indentations      = new Array[Int](8)
  private[this] var indentationSize   = 0
  private[this] var flowSequenceLevel = 0
  private[this] var flowMappingLevel  = 0

  def hasNoIndent: Boolean = indentationSize == 0 || indentations(indentationSize - 1) == 0
  def indent: Int          = if (indentationSize == 0) -1 else indentations(indentationSize - 1)
  def addIndent(newIndent: Int): Unit = {
    val i = indentationSize
    if (i == indentations.length) indentations = java.util.Arrays.copyOf(indentations, i << 1)
    indentations(i) = newIndent
    indentationSize += 1
  }
  def removeLastIndent(): Unit = if (indentationSize > 0) indentationSize -= 1

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
      removeLastIndent()
      new Token(BlockEnd, reader.range) :: checkIndents(current)
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
    checkIndents(-1) :+ new Token(DocumentStart, reader.range)

  def parseDocumentEnd(): List[Token] =
    popPotentialKeys() ++ checkIndents(-1) :+ new Token(DocumentEnd, reader.range)
}

private[reader] object TokenizerContext {
  def apply(in: String): TokenizerContext = new TokenizerContext(new StringReader(in))
}
