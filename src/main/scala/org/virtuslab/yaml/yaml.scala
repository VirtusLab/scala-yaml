package org.virtuslab.scala.yaml

import org.virtuslab.internal.YamlError
import org.virtuslab.internal.load.compose.ComposerImpl
import org.virtuslab.internal.load.compose.Node
import org.virtuslab.yaml.YamlEncoder
import org.virtuslab.yaml.Util

import scala.deriving._
import org.virtuslab.internal.load.construct.Construct

inline def deriveEncoder[T](using m: Mirror.Of[T]): YamlEncoder[T] = YamlEncoder.derived[T]
inline def deriveDecoder[T](using m: Mirror.Of[T]): Construct[T]   = Construct.derived[T]

extension (str: String) def as[T](using c: Construct[T]): Either[YamlError, T] = Util.as(str)
extension [T](t: T) def asYaml(using encoder: YamlEncoder[T]): String          = Util.asYaml(t)
