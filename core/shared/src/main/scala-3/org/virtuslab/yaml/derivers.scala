package org.virtuslab.yaml

import scala.deriving.Mirror

inline def deriveYamlEncoder[T](using m: Mirror.Of[T]): YamlEncoder[T] = YamlEncoder.derived[T]
inline def deriveYamlDecoder[T](using m: Mirror.Of[T]): YamlDecoder[T] = YamlDecoder.derived[T]
inline def deriveYamlCodec[T](using m: Mirror.Of[T]): YamlCodec[T]     = YamlCodec.derived[T]
