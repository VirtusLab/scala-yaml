package org.virtuslab

import org.virtuslab.internal.load.compose.ComposerImpl

import scala.deriving._

object Yaml {
  type YamlDecoder[T] = org.virtuslab.internal.load.construct.YamlDecoder[T]
  final val YamlDecoder = org.virtuslab.internal.load.construct.YamlDecoder

  type YamlEncoder[T] = org.virtuslab.internal.dump.YamlEncoder[T]
  final val YamlEncoder = org.virtuslab.internal.dump.YamlEncoder

  type Node = org.virtuslab.internal.load.compose.Node
  final val Node = org.virtuslab.internal.load.compose.Node

  type YamlError = org.virtuslab.internal.YamlError

  inline def deriveEncoder[T](using m: Mirror.Of[T]): YamlEncoder[T] = YamlEncoder.derived[T]
  inline def deriveDecoder[T](using m: Mirror.Of[T]): YamlDecoder[T] = YamlDecoder.derived[T]

  extension (str: String)
    def as[T](using c: YamlDecoder[T]): Either[YamlError, T] =
      for
        node <- ComposerImpl.compose(str)
        t    <- c.construct(node)
      yield t

  extension [T](t: T) def asYaml(using encoder: YamlEncoder[T]): String = encoder.toYaml(t)
}
