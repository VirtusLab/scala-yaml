package org.virtuslab.internal.load.construct

import scala.deriving._
import scala.compiletime._
import org.virtuslab.internal.load.compose.Node

sealed trait ConstructError

trait Construct[T]:
  def construct(node: Node): Either[ConstructError, T]

object Construct:
  inline def derived[T](using m: Mirror.Of[T]): Construct[T] =
    new Construct[T] {
      def construct(node: Node): Either[ConstructError, T] = ???
    }
