package org.virtuslab.yaml

import org.virtuslab.internal.YamlError
import org.virtuslab.internal.load.compose.ComposerImpl
import org.virtuslab.internal.load.compose.Node
import org.virtuslab.internal.load.construct.Construct
import org.virtuslab.internal.load.parse.ParserImpl

import scala.compiletime._
import scala.deriving._

trait YamlDecoder[T]:
  final def from(yaml: String): Either[YamlError, T] =
    for
      node <- ComposerImpl.compose(yaml)
      t    <- apply(node)
    yield t

  def apply(node: Node): Either[YamlError, T]

object YamlDecoder:
  inline def derived[T](using m: Mirror.Of[T]): YamlDecoder[T] =
    val c = Construct.derived[T]
    new YamlDecoder[T] {
      def apply(node: Node): Either[YamlError, T] = c.construct(node)
    }
