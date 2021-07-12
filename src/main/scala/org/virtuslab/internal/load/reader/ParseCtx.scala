package org.virtuslab.internal.load.reader

import org.virtuslab.internal.load.reader
import org.virtuslab.internal.load.reader.StateStack
import token.Token

case class ReaderCtx(val stateStack: StateStack) {

  def appendSequence(indent: Int): Token =
    stateStack.peek() match {
      case Some(ReaderState.Sequence(i)) if i > indent =>
        stateStack.pop()
        Token.SequenceEnd
      case Some(ReaderState.Mapping(i)) if i > indent =>
        stateStack.pop()
        Token.MappingEnd
      case _ =>
        stateStack.push(ReaderState.Sequence(indent))
        Token.SequenceStart
    }

  def appendMapping(indent: Int): Token =
    stateStack.peek() match {
      case Some(ReaderState.Sequence(i)) if i >= indent =>
        stateStack.pop()
        Token.SequenceEnd
      case Some(ReaderState.Mapping(i)) if i > indent =>
        stateStack.pop()
        Token.MappingEnd
      case _ =>
        stateStack.push(ReaderState.Mapping(indent))
        Token.MappingStart
    }

  def popTokenFromStack: Token = {
    stateStack.pop() match {
      case Some(ReaderState.Sequence(_)) =>
        Token.SequenceEnd
      case Some(ReaderState.Mapping(_)) =>
        Token.MappingEnd
      case Some(ReaderState.Document(_)) =>
        Token.DocumentEnd
      case _ => Token.StreamEnd
    }
  }

  def shouldParseSequenceNode(indent: Int): Boolean = {
    stateStack.peek() match {
      case Some(ReaderState.Sequence(i)) if i == indent => true
      case _                                            => false
    }
  }

  def shouldParseMappingNode(indent: Int): Boolean = {
    stateStack.peek() match {
      case Some(ReaderState.Mapping(i)) if i == indent => true
      case _                                           => false
    }
  }
}

case object ReaderCtx:
  def init: ReaderCtx = ReaderCtx(StateStack(List(ReaderState.Document(0))))
