package org.virtuslab.yaml

import org.virtuslab.yaml
import org.virtuslab.yaml.internal.dump.present.PresenterImpl
import org.virtuslab.yaml.internal.dump.serialize.SerializerImpl
import org.virtuslab.yaml.internal.load.compose.ComposerImpl

import scala.deriving.Mirror

inline def deriveEncoder[T](using m: Mirror.Of[T]): YamlEncoder[T] = YamlEncoder.derived[T]
inline def deriveDecoder[T](using m: Mirror.Of[T]): YamlDecoder[T] = YamlDecoder.derived[T]

extension (str: String)
  def as[T](using c: YamlDecoder[T]): Either[YamlError, T] =
    for
      node <- ComposerImpl.compose(str)
      t    <- c.construct(node)
    yield t

extension [T](t: T)
  def asYaml(using encoder: YamlEncoder[T]): String =
    val events = SerializerImpl.toEvents(encoder.asNode(t))
    PresenterImpl.asString(events)
