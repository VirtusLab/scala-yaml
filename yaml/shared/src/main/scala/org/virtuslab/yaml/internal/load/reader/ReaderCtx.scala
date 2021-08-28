package org.virtuslab.yaml.internal.load.reader

import org.virtuslab.yaml.internal.load.reader
import org.virtuslab.yaml.internal.load.reader.Reader
import org.virtuslab.yaml.internal.load.reader.StringReader

import scala.annotation.tailrec
import scala.collection.mutable

import token.Token
case class ReaderCtx(
    stateStack: mutable.Stack[ReaderState],
    tokens: mutable.ArrayDeque[Token] = mutable.ArrayDeque.empty,
    reader: Reader
) {

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

  def appendState(state: ReaderState): Unit = stateStack.push(state)

  def closeOpenedFlowMapping(): List[Token] = stateStack.headOption match
    case Some(ReaderState.FlowMapping) =>
      stateStack.pop()
      List(Token.FlowMappingEnd(reader.pos()))
    case _ =>
      Nil

  def closeOpenedSequence(): List[Token] =
    stateStack.headOption match
      case Some(ReaderState.Sequence(_)) =>
        stateStack.pop()
        List(Token.SequenceEnd(reader.pos()))
      case Some(ReaderState.FlowSequence) =>
        stateStack.pop()
        List(Token.FlowSequenceEnd(reader.pos()))
      case _ =>
        Nil

  def shouldParseSequenceEntry(indent: Int): Boolean =
    stateStack.headOption match
      case Some(ReaderState.Sequence(i)) if i == indent => true
      case _                                            => false

  def shouldParseMappingEntry(indent: Int): Boolean =
    stateStack.headOption match
      case Some(ReaderState.Mapping(i)) if i == indent => true
      case _                                           => false

  def isAllowedSpecialCharacter(char: Char): Boolean =
    stateStack.headOption match
      case Some(ReaderState.FlowMapping) if char == '}'                                  => false
      case Some(ReaderState.FlowMapping) | Some(ReaderState.FlowSequence) if char == ',' => false
      case Some(ReaderState.FlowSequence) if char == ']'                                 => false
      case _                                                                             => true

  def isFlowMapping(): Boolean =
    stateStack.headOption match
      case Some(ReaderState.FlowMapping) => true
      case _                             => false

  def closeOpenedScopes(): List[Token] =
    @tailrec
    def loop(acc: List[Token]): List[Token] =
      stateStack.headOption match
        case Some(ReaderState.Sequence(_)) =>
          stateStack.pop()
          loop(acc :+ Token.SequenceEnd(reader.pos()))
        case Some(ReaderState.Mapping(_)) =>
          stateStack.pop()
          loop(acc :+ Token.MappingEnd(reader.pos()))
        case _ => acc

    loop(Nil)

  def parseDocumentStart(): List[Token] =
    val closedScopes = closeOpenedScopes()
    stateStack.push(ReaderState.Document)
    closedScopes :+ Token.DocumentStart(reader.pos())

  def parseDocumentEnd(): List[Token] =
    closeOpenedScopes() :+ Token.DocumentEnd(reader.pos())
}

case object ReaderCtx:
  def init(in: String): ReaderCtx =
    ReaderCtx(mutable.Stack.empty, mutable.ArrayDeque.empty, StringReader(in))
