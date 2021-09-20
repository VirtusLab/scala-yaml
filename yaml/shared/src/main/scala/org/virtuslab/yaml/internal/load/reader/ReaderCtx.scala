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

  def getIndentOfLatestCollection(): Option[Int] =
    stateStack.headOption.map(_.indent)

  def appendState(state: ReaderState): Unit = stateStack.push(state)

  def closeOpenedFlowMapping(): List[Token] = stateStack.headOption match
    case Some(ReaderState.FlowMapping(_)) =>
      stateStack.pop()
      List(Token.FlowMappingEnd(reader.pos()))
    case _ =>
      Nil

  def closeOpenedFlowSequence(): List[Token] =
    stateStack.headOption match
      case Some(ReaderState.FlowSequence(_)) =>
        stateStack.pop()
        List(Token.FlowSequenceEnd(reader.pos()))
      case _ =>
        val token = closeOpenedCollection()
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

  private def closeOpenedCollection(): Token =
    val pos = reader.pos()
    stateStack.removeHead() match
      case _: ReaderState.Sequence => Token.SequenceEnd(pos)
      case _: ReaderState.Mapping => Token.MappingEnd(pos)
      case _: ReaderState.FlowSequence => Token.FlowSequenceEnd(pos)
      case _: ReaderState.FlowMapping => Token.FlowMappingEnd(pos)
      case _: ReaderState.Document => Token.DocumentEnd(pos)

  /**
   * Check if given character is allowed and can be part of being read scalar
   */
  def isAllowedSpecialCharacter(char: Char): Boolean =
    val flowMapping = stateStack.exists(_.isInstanceOf[ReaderState.FlowMapping])
    val flowSequence = stateStack.exists(_.isInstanceOf[ReaderState.FlowSequence])
    if (flowMapping && char == '}') false
    else if ((flowMapping || flowSequence) && char == ',') false
    else if (flowSequence && char == ']') false
    else true

  def isFlowMapping(): Boolean =
    stateStack.headOption match
      case Some(ReaderState.FlowMapping(_)) => true
      case _                                => false

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

  def parseDocumentStart(indent: Int): List[Token] =
    val closedScopes = closeOpenedScopes()
    stateStack.push(ReaderState.Document(indent))
    closedScopes :+ Token.DocumentStart(reader.pos())

  def parseDocumentEnd(): List[Token] =
    closeOpenedScopes() :+ Token.DocumentEnd(reader.pos())
}

case object ReaderCtx:
  def init(in: String): ReaderCtx =
    ReaderCtx(mutable.Stack.empty, mutable.ArrayDeque.empty, StringReader(in))
