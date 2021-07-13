package org.virtuslab.yaml

import scala.deriving._
import scala.compiletime._
import org.virtuslab.internal.load.parse.ParserImpl
import org.virtuslab.internal.load.construct.Construct
import org.virtuslab.internal.load.compose.Node
import org.virtuslab.internal.YamlError
import org.virtuslab.internal.load.compose.ComposerImpl
import org.virtuslab.internal.load.reader.YamlReader

trait YamlDecoder[T: Mirror.Of]:

  given yamlWriter: Construct[T] = Construct.derived[T]

  final def from(yaml: String): Either[YamlError, T] =
    for node <- ComposerImpl.compose(yaml)
    yield apply(node)

  def apply(node: Node): T = ???

object YamlDecoder:
  inline def derived[T](using m: Mirror.Of[T]): YamlDecoder[T] = {
    new YamlDecoder[T] {
      override def apply(node: Node): T = ???
    }
  }
