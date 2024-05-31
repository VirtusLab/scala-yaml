package org.virtuslab.yaml

/**
 * A type class that provides both-way conversion between [[Node]] and [[T]]
 */
trait YamlCodec[T] extends YamlDecoder[T] with YamlEncoder[T]

object YamlCodec extends YamlCodecCompanionCrossCompat {

  def apply[T](implicit self: YamlCodec[T]): YamlCodec[T] = self

}
