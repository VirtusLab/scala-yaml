package org.virtuslab.scala.yaml

import org.virtuslab.yaml.{YamlDecoder, YamlEncoder}
import org.virtuslab.internal.load.compose.Node
import org.virtuslab.internal.load.compose.ComposerImpl
import org.virtuslab.internal.YamlError

import scala.deriving._

def deriveEncoder[T](using m: Mirror.Of[T]) = YamlEncoder.derived
def deriveDecoder[T](using m: Mirror.Of[T]) = YamlDecoder.derived

def parse(yaml: String): Either[YamlError, Node] = ComposerImpl.compose(yaml)
extension (str: String)
  def fromYaml[T](using decoder: YamlDecoder[T]): Either[Any, T] = decoder.from(str)

extension [T](t: T) def asYaml(using encoder: YamlEncoder[T]): String = encoder.toYaml(t)
