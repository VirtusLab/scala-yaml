package org.virtuslab.yaml

import scala.deriving.Mirror

private[yaml] trait YamlCodecCompanionCrossCompat {
  inline def derived[T](using m: Mirror.Of[T]): YamlCodec[T] =
    new YamlCodec[T]:
      val decoder = YamlDecoder.derived[T]
      val encoder = YamlEncoder.derived[T]

      def construct(node: Node)(using
          settings: LoadSettings = LoadSettings.empty
      ): Either[ConstructError, T] = decoder.construct(node)
      def asNode(obj: T): Node = encoder.asNode(obj)
}
