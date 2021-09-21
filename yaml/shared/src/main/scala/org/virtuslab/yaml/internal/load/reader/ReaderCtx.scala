package org.virtuslab.yaml.internal.load.reader

import org.virtuslab.yaml.Position
import org.virtuslab.yaml.internal.load.reader.Reader
import org.virtuslab.yaml.internal.load.reader.StringReader

import scala.annotation.tailrec
import scala.collection.mutable

import token.Token

final case class ReaderCtx(
    stateStack: mutable.Stack[ReaderState],
    tokens: mutable.ArrayDeque[Token] = mutable.ArrayDeque.empty,
    reader: Reader
) {

  @tailrec
  def closeOpenedCollectionSequences(indent: Int): Unit =
    stateStack.headOption match
      case Some(ReaderState.Sequence(i)) if i > indent =>
        stateStack.pop()
        tokens.append(Token.SequenceEnd(reader.pos()))
        closeOpenedCollectionSequences(indent)
      case Some(ReaderState.Mapping(i)) if i > indent =>
        stateStack.pop()
        tokens.append(Token.MappingEnd(reader.pos()))
        closeOpenedCollectionSequences(indent)
      case _ => ()

  @tailrec
  def closeOpenedCollectionMapping(indent: Int): Unit =
    stateStack.headOption match
      case Some(ReaderState.Sequence(i)) if i >= indent =>
        stateStack.pop()
        tokens.append(Token.SequenceEnd(reader.pos()))
        closeOpenedCollectionMapping(indent)
      case Some(ReaderState.Mapping(i)) if i > indent =>
        stateStack.pop()
        tokens.append(Token.MappingEnd(reader.pos()))
        closeOpenedCollectionMapping(indent)
      case _ => ()

  def getIndentOfLatestCollection(): Option[Int] =
    stateStack.headOption.map(_.indent)

  def appendState(state: ReaderState): Unit = stateStack.push(state)

  // todo needs similar logic as closeOpenedFlowSequence. Fix that and add example for docstring.
  def closeOpenedFlowMapping(): List[Token] = stateStack.headOption match
    case Some(ReaderState.FlowMapping(_)) =>
      stateStack.pop()
      List(Token.FlowMappingEnd(reader.pos()))
    case _ =>
      Nil

  /**
   * Try to close currently opened flow sequence.
   * This action might close additional sequence, consider following case: for yaml "[ k: v,<<parser>>]" 
   * we have to close mapping related to the "k: v" and only then we can close flow sequence
   */
  @tailrec
  def closeOpenedFlowSequence(): List[Token] =
    stateStack.headOption match
      case Some(ReaderState.FlowSequence(_)) =>
        stateStack.pop()
        List(Token.FlowSequenceEnd(reader.pos()))
      case _ =>
        val token = unsafeCloseOpenedCollection()
        tokens.append(token)
        closeOpenedFlowSequence()

  def shouldParseSequenceEntry(indent: Int): Boolean =
    stateStack.headOption match
      case Some(ReaderState.Sequence(i)) if i == indent => true
      case _                                            => false

  def shouldParseMappingEntry(indent: Int): Boolean =
    stateStack.headOption match
      case Some(ReaderState.Mapping(i)) if i == indent => true
      case _                                           => false

  /**
   * Closes ONE opened collection. Unsafe because caller has to guarantee that there exists at least one opened collection
   */
  private def unsafeCloseOpenedCollection(): Token =
    val pos = reader.pos()
    stateStack.removeHead() match
      case _: ReaderState.Sequence     => Token.SequenceEnd(pos)
      case _: ReaderState.Mapping      => Token.MappingEnd(pos)
      case _: ReaderState.FlowSequence => Token.FlowSequenceEnd(pos)
      case _: ReaderState.FlowMapping  => Token.FlowMappingEnd(pos)

  /**
   * Checks if given character is allowed and can be a part of being read scalar
   * Checking last collection isn't sufficient, we have to traverse all of them.
   */
  def isAllowedSpecialCharacter(char: Char): Boolean =
    val flowMapping  = stateStack.exists(_.isInstanceOf[ReaderState.FlowMapping])
    val flowSequence = stateStack.exists(_.isInstanceOf[ReaderState.FlowSequence])
    if (flowMapping && char == '}') false
    else if ((flowMapping || flowSequence) && char == ',') false
    else if (flowSequence && char == ']') false
    else true

  def isFlowMapping(): Boolean =
    stateStack.headOption match
      case Some(ReaderState.FlowMapping(_)) => true
      case _                                => false

  /**
   * Traverses through currently opened collections and maps them to the corresponding Token.<collection>End token
   */
  private def closeOpenedCollections(): List[Token] =
    @tailrec
    def loop(acc: mutable.ArrayDeque[Token]): List[Token] =
      if stateStack.size > 0 then loop(acc.append(unsafeCloseOpenedCollection()))
      else acc.toList

    loop(new mutable.ArrayDeque(stateStack.size + 1))

  def parseDocumentStart(indent: Int): List[Token] =
    val closedScopes = closeOpenedCollections()
    closedScopes :+ Token.DocumentStart(reader.pos())

  def parseDocumentEnd(): List[Token] =
    closeOpenedCollections() :+ Token.DocumentEnd(reader.pos())

  def parseStreamEnd(): List[Token] =
    closeOpenedCollections() :+ Token.StreamEnd(reader.pos())
}

case object ReaderCtx:
  def init(in: String): ReaderCtx =
    ReaderCtx(mutable.Stack.empty, mutable.ArrayDeque.empty, StringReader(in))
