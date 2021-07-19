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

  def closeOpenedCollectionForSequences(indent: Int): Unit =
    stateStack.peek() match
      case Some(ReaderState.Sequence(i)) if i > indent =>
        stateStack.pop()
        tokens.append(Token.SequenceEnd)
        closeOpenedCollectionForSequences(indent)
      case Some(ReaderState.Mapping(i)) if i > indent =>
        stateStack.pop()
        tokens.append(Token.MappingEnd)
        closeOpenedCollectionForSequences(indent)
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

  def appendSequence(indent: Int): Token =
    stateStack.push(ReaderState.Sequence(indent))
    Token.SequenceStart

  def appendMapping(indent: Int): Token =
    stateStack.push(ReaderState.Mapping(indent))
    Token.MappingStart

  def popTokenFromStack: Token =
    stateStack.pop() match
      case Some(ReaderState.Sequence(_)) => Token.SequenceEnd
      case Some(ReaderState.Mapping(_))  => Token.MappingEnd
      case Some(ReaderState.Document)    => popTokenFromStack
      case _                             => Token.StreamEnd

  def shouldParseSequenceEntry(indent: Int): Boolean =
    stateStack.peek() match
      case Some(ReaderState.Sequence(i)) if i == indent => true
      case _                                            => false

  def shouldParseMappingEntry(indent: Int): Boolean =
    stateStack.peek() match
      case Some(ReaderState.Mapping(i)) if i == indent => true
      case _                                           => false

  private def closeOpenedScopes(): List[Token] =
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
