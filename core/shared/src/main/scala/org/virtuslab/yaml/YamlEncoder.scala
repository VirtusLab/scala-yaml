package org.virtuslab.yaml

/**
 * A type class that provides a conversion from a given type [[T]] into [[Node]]
 */
trait YamlEncoder[T] { self =>
  def asNode(obj: T): Node

  final def mapContra[T1](f: T1 => T): YamlEncoder[T1] = new YamlEncoder[T1] {
    override def asNode(obj: T1): Node = self.asNode(f(obj))
  }
}

object YamlEncoder extends YamlEncoderCrossCompanionCompat {

  def apply[T](implicit self: YamlEncoder[T]): YamlEncoder[T] = self

  implicit def forByte: YamlEncoder[Byte]       = v => Node.ScalarNode(v.toString)
  implicit def forChar: YamlEncoder[Char]       = v => Node.ScalarNode(v.toString)
  implicit def forShort: YamlEncoder[Short]     = v => Node.ScalarNode(v.toString)
  implicit def forInt: YamlEncoder[Int]         = v => Node.ScalarNode(v.toString)
  implicit def forLong: YamlEncoder[Long]       = v => Node.ScalarNode(v.toString)
  implicit def forFloat: YamlEncoder[Float]     = v => Node.ScalarNode(v.toString)
  implicit def forDouble: YamlEncoder[Double]   = v => Node.ScalarNode(v.toString)
  implicit def forBoolean: YamlEncoder[Boolean] = v => Node.ScalarNode(v.toString)
  implicit def forString: YamlEncoder[String]   = v => Node.ScalarNode(v)

  implicit def forSet[T](implicit encoder: YamlEncoder[T]): YamlEncoder[Set[T]] = (nodes) =>
    Node.SequenceNode(nodes.map(encoder.asNode(_)).toSeq, Tag.seq)

  implicit def forSeq[T](implicit encoder: YamlEncoder[T]): YamlEncoder[Seq[T]] = (nodes) =>
    Node.SequenceNode(nodes.map(encoder.asNode(_)), Tag.seq)

  implicit def forList[T](implicit encoder: YamlEncoder[T]): YamlEncoder[List[T]] = (nodes) =>
    Node.SequenceNode(nodes.map(encoder.asNode(_)), Tag.seq)

  // todo support arbitrary node as key in KeyValueNode
  implicit def forMap[K, V](implicit
      keyCodec: YamlEncoder[K],
      valueCodec: YamlEncoder[V]
  ): YamlEncoder[Map[K, V]] = { (nodes) =>
    val mappings = nodes.map { case (key, value) =>
      (keyCodec.asNode(key) -> valueCodec.asNode(value))
    }
    Node.MappingNode(mappings)
  }

}
