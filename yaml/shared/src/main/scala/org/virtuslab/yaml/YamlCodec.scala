package org.virtuslab.yaml

import scala.compiletime._
import scala.deriving._

import org.virtuslab.yaml.*

/**
 * A type class that provides both-way conversion between [[Node]] and [[T]]
 */
trait YamlCodec[T] extends YamlDecoder[T] with YamlEncoder[T]

object YamlCodec:
  inline def derived[T](using m: Mirror.Of[T]): YamlCodec[T] =
    new YamlCodec[T]:
      val decoder = YamlDecoder.derived[T]
      val encoder = YamlEncoder.derived[T]

      def construct(node: Node)(using
          settings: LoadSettings = LoadSettings.empty
      ): Either[ConstructError, T] = decoder.construct(node)
      def asNode(obj: T): Node     = encoder.asNode(obj)

end YamlCodec
