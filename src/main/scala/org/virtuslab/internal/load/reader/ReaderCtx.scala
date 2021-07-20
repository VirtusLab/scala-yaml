package org.virtuslab.internal.load.reader

import org.virtuslab.internal.load.reader
import org.virtuslab.internal.load.reader.ReaderStack
import token.Token

import scala.annotation.tailrec
import scala.collection.mutable.ArrayDeque

case class ReaderCtx(
    stateStack: ReaderStack,
    tokens: ArrayDeque[Token] = ArrayDeque.empty
) {

  def closeOpenedCollectionSequences(indent: Int): Unit =
    stateStack.peek() match
      case Some(ReaderState.Sequence(i)) if i > indent =>
        stateStack.pop()
        tokens.append(Token.SequenceEnd)
        closeOpenedCollectionSequences(indent)
      case Some(ReaderState.Mapping(i)) if i > indent =>
        stateStack.pop()
        tokens.append(Token.MappingEnd)
        closeOpenedCollectionSequences(indent)
      case _ => ()

  def closeOpenedCollectionMapping(indent: Int): Unit =
    stateStack.peek() match
      case Some(ReaderState.Sequence(i)) if i >= indent =>
        stateStack.pop()
        tokens.append(Token.SequenceEnd)
        closeOpenedCollectionMapping(indent)
      case Some(ReaderState.Mapping(i)) if i > indent =>
        stateStack.pop()
        tokens.append(Token.MappingEnd)
        closeOpenedCollectionMapping(indent)
      case _ => ()

  def appendState(state: ReaderState): Unit = stateStack.push(state)

  def closeOpenedFlowMapping(): List[Token] =
    stateStack.pop() match
      case Some(ReaderState.FlowMapping) => List(Token.FlowMappingEnd)
      case _                             => Nil

  def closeOpenedSequence(): List[Token] =
    stateStack.pop() match
      case Some(ReaderState.Sequence(_) | ReaderState.FlowSequence) => List(Token.SequenceEnd)
      case _                                                        => Nil

  def shouldParseSequenceEntry(indent: Int): Boolean =
    stateStack.peek() match
      case Some(ReaderState.Sequence(i)) if i == indent => true
      case _                                            => false

  def shouldParseMappingEntry(indent: Int): Boolean =
    stateStack.peek() match
      case Some(ReaderState.Mapping(i)) if i == indent => true
      case _                                           => false

  def isAllowedSpecialCharacter(char: Char): Boolean =
    stateStack.peek() match
      case Some(ReaderState.FlowMapping) if char == '}'                                  => false
      case Some(ReaderState.FlowMapping) | Some(ReaderState.FlowSequence) if char == ',' => false
      case Some(ReaderState.FlowSequence) if char == ']'                                 => false
      case _                                                                             => true

  def isFlowMapping(): Boolean =
    stateStack.peek() match
      case Some(ReaderState.FlowMapping) => true
      case _                             => false

  def closeOpenedScopes(): List[Token] =
    @tailrec
    def loop(acc: List[Token]): List[Token] =
      stateStack.peek() match
        case Some(ReaderState.Sequence(_)) =>
          stateStack.pop()
          loop(acc :+ Token.SequenceEnd)
        case Some(ReaderState.Mapping(_)) =>
          stateStack.pop()
          loop(acc :+ Token.MappingEnd)
        case _ => acc

    loop(Nil)

  def parseDocumentStart(): List[Token] =
    val closedScopes = closeOpenedScopes()
    stateStack.push(ReaderState.Document)
    closedScopes :+ Token.DocumentStart

  def parseDocumentEnd(): List[Token] =
    closeOpenedScopes() :+ Token.DocumentEnd
}

case object ReaderCtx:
  def init: ReaderCtx = ReaderCtx(ReaderStack(Nil))
