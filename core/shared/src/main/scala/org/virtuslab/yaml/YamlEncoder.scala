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

  implicit def forByte: YamlEncoder[Byte]       = v => new Node.ScalarNode(v.toString, Tag.int)
  implicit def forChar: YamlEncoder[Char]       = v => new Node.ScalarNode(v.toString, Tag.str)
  implicit def forShort: YamlEncoder[Short]     = v => new Node.ScalarNode(v.toString, Tag.int)
  implicit def forInt: YamlEncoder[Int]         = v => new Node.ScalarNode(v.toString, Tag.int)
  implicit def forLong: YamlEncoder[Long]       = v => new Node.ScalarNode(v.toString, Tag.int)
  implicit def forFloat: YamlEncoder[Float]     = v => new Node.ScalarNode(v.toString, Tag.float)
  implicit def forDouble: YamlEncoder[Double]   = v => new Node.ScalarNode(v.toString, Tag.float)
  implicit def forBoolean: YamlEncoder[Boolean] = v => new Node.ScalarNode(v.toString, Tag.boolean)
  implicit def forString: YamlEncoder[String]   = v => new Node.ScalarNode(v, Tag.str)

  implicit def forOption[T](implicit encoder: YamlEncoder[T]): YamlEncoder[Option[T]] = {
    case Some(t) => encoder.asNode(t)
    case _       => new Node.ScalarNode("", Tag.nullTag)
  }

  implicit def forSet[T](implicit encoder: YamlEncoder[T]): YamlEncoder[Set[T]] = nodes =>
    Node.SequenceNode(
      nodes.foldLeft(Seq.newBuilder[Node])((acc, n) => acc.addOne(encoder.asNode(n))).result(),
      Tag.seq
    )

  implicit def forSeq[T](implicit encoder: YamlEncoder[T]): YamlEncoder[Seq[T]] = nodes =>
    Node.SequenceNode(nodes.map(encoder.asNode), Tag.seq)

  implicit def forList[T](implicit encoder: YamlEncoder[T]): YamlEncoder[List[T]] = nodes =>
    Node.SequenceNode(nodes.map(encoder.asNode), Tag.seq)

  implicit def forMap[K, V](implicit
      keyCodec: YamlEncoder[K],
      valueCodec: YamlEncoder[V]
  ): YamlEncoder[Map[K, V]] =
    nodes => Node.MappingNode(nodes.map(kv => (keyCodec.asNode(kv._1), valueCodec.asNode(kv._2))))
}
