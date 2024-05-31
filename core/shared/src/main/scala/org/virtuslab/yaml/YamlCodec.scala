package org.virtuslab.yaml

/**
 * A type class that provides both-way conversion between [[Node]] and [[T]]
 */
trait YamlCodec[T] extends YamlDecoder[T] with YamlEncoder[T] { self =>

  def imap[T1](f: T => T1)(g: T1 => T): YamlCodec[T1] =
    YamlCodec.from(self.map(f), self.contramap(g))

  def iemap[T1](f: T => Either[ConstructError, T1])(g: T1 => T): YamlCodec[T1] =
    YamlCodec.from(self.flatMap(f), self.contramap(g))
}

object YamlCodec extends YamlCodecCompanionCrossCompat {

  def apply[T](implicit self: YamlCodec[T]): YamlCodec[T] = self

  def from[A](decoder: YamlDecoder[A], encoder: YamlEncoder[A]): YamlCodec[A] = new YamlCodec[A] {

    override def construct(node: Node)(implicit settings: LoadSettings): Either[ConstructError, A] =
      decoder.construct(node)

    override def asNode(obj: A): Node =
      encoder.asNode(obj)
  }
}
