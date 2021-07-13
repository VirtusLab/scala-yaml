package org.virtuslab.scala.yaml

import org.virtuslab.internal.YamlError
import org.virtuslab.internal.load.compose.ComposerImpl
import org.virtuslab.internal.load.compose.Node
import org.virtuslab.yaml.YamlDecoder
import org.virtuslab.yaml.YamlEncoder

import scala.deriving._

inline def deriveEncoder[T](using m: Mirror.Of[T]): YamlEncoder[T] = YamlEncoder.derived[T]
inline def deriveDecoder[T](using m: Mirror.Of[T]): YamlDecoder[T] = YamlDecoder.derived[T]

extension (str: String)
  def as[T](using decoder: YamlDecoder[T]): Either[YamlError, T]      = decoder.from(str)
extension [T](t: T) def asYaml(using encoder: YamlEncoder[T]): String = encoder.toYaml(t)
