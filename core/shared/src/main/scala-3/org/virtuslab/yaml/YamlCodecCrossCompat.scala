package org.virtuslab.yaml

import scala.deriving.Mirror

private[yaml] trait YamlCodecCompanionCrossCompat {

  def make[A](implicit decoder: YamlDecoder[A], encoder: YamlEncoder[A]): YamlCodec[A]

  inline def derived[T](using m: Mirror.Of[T]): YamlCodec[T] =
    val decoder = YamlDecoder.derived[T]
    val encoder = YamlEncoder.derived[T]
    make(decoder, encoder)
}
