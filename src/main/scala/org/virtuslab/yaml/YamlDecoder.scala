package org.virtuslab.yaml

import scala.deriving._
import scala.compiletime._

import org.virtuslab.internal.YamlError
import org.virtuslab.internal.load.{YamlReader, StringYamlReader}
import org.virtuslab.internal.load.parse.ParserImpl
import org.virtuslab.internal.load.construct.{Der}
import org.virtuslab.internal.load.compose.{Node, ComposerImpl}

trait YamlDecoder[T: Mirror.Of]:
  final def from(yaml: String): Either[YamlError, T] =
    for
      node <- ComposerImpl.compose(yaml)
      t    <- apply(node)
    yield t

  def apply(node: Node): Either[YamlError, T]
