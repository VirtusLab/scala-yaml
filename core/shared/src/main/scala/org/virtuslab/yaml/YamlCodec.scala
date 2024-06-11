package org.virtuslab.yaml

/**
 * A type class that provides both-way conversion between [[Node]] and [[T]]
 */
trait YamlCodec[T] extends YamlDecoder[T] with YamlEncoder[T] { self =>

  def mapInvariant[T1](f: T => T1)(g: T1 => T): YamlCodec[T1] =
    YamlCodec.make(self.map(f), self.mapContra(g))

  def mapInvariantError[T1](f: T => Either[ConstructError, T1])(g: T1 => T): YamlCodec[T1] =
    YamlCodec.make(self.mapError(f), self.mapContra(g))
}

object YamlCodec extends YamlCodecCompanionCrossCompat {

  def apply[T](implicit self: YamlCodec[T]): YamlCodec[T] = self

  def make[A](implicit decoder: YamlDecoder[A], encoder: YamlEncoder[A]): YamlCodec[A] =
    new YamlCodec[A] {

      override def construct(node: Node)(implicit
          settings: LoadSettings
      ): Either[ConstructError, A] =
        decoder.construct(node)

      override def asNode(obj: A): Node =
        encoder.asNode(obj)
    }
}
