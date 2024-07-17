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
  // Define the allowed exceptions in the otherwise disallowed ranges
  private val allowedExceptions = Set('\u0009', '\u000A', '\u000D', '\u0085')

  def isCharNonPrintable(c: Char): Boolean = {
    if (allowedExceptions.contains(c)) false
    else {
      (c >= '\u0000' && c <= '\u001F') || // C0 control block (except allowed exceptions above)
      c == '\u007F' ||                    // DEL
      (c >= '\u0080' && c <= '\u009F') || // C1 control block (except for NEL \u0085)
      (c >= '\uD800' && c <= '\uDFFF') || // Surrogate block
      c == '\uFFFE' || c == '\uFFFF'      // Disallowed specific characters
    }
  }

  def escapeSpecialCharacters(scalar: String): String =
    scalar.flatMap { char =>
      if (isCharNonPrintable(char))
        f"\\u${char.toInt}%04X"
      else
        char.toString
    }

  def apply[T](implicit self: YamlEncoder[T]): YamlEncoder[T] = self

  implicit def forByte: YamlEncoder[Byte]       = v => Node.ScalarNode(v.toString)
  implicit def forChar: YamlEncoder[Char]       = v => Node.ScalarNode(v.toString)
  implicit def forShort: YamlEncoder[Short]     = v => Node.ScalarNode(v.toString)
  implicit def forInt: YamlEncoder[Int]         = v => Node.ScalarNode(v.toString)
  implicit def forLong: YamlEncoder[Long]       = v => Node.ScalarNode(v.toString)
  implicit def forFloat: YamlEncoder[Float]     = v => Node.ScalarNode(v.toString)
  implicit def forDouble: YamlEncoder[Double]   = v => Node.ScalarNode(v.toString)
  implicit def forBoolean: YamlEncoder[Boolean] = v => Node.ScalarNode(v.toString)
  implicit def forString: YamlEncoder[String]   = v => Node.ScalarNode(escapeSpecialCharacters(v))

  implicit def forOption[T](implicit encoder: YamlEncoder[T]): YamlEncoder[Option[T]] = {
    case Some(t) => encoder.asNode(t)
    case None    => Node.ScalarNode("", Tag.nullTag)
  }

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
