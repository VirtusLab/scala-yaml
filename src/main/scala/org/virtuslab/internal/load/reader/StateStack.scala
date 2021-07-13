package org.virtuslab.internal.load.reader

case class StateStack(var stack: List[ReaderState]):

  def push(state: ReaderState) =
    stack = state :: stack

  def peek(): Option[ReaderState] = stack.headOption

  def pop(): Option[ReaderState] =
    stack.headOption match
      case Some(v) =>
        stack = stack.tail
        Some(v)
      case _ => None
