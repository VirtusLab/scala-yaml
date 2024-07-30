package org.virtuslab.yaml

import scala.reflect.ClassTag
import scala.util.Try

import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Node._

/**
 * A type class that provides a conversion from a [[Node]] into given type [[T]]
 */
trait YamlDecoder[T] { self =>
  def construct(node: Node)(implicit
      settings: LoadSettings = LoadSettings.empty
  ): Either[ConstructError, T]

  final def orElse[T1 >: T](that: => YamlDecoder[T1]): YamlDecoder[T1] = new YamlDecoder[T1] {
    override def construct(
        node: Node
    )(implicit settings: LoadSettings): Either[ConstructError, T1] =
      self.construct(node) match {
        case result @ Right(_) => result
        case Left(_)           => that.construct(node)
      }
  }

  final def widen[T1 >: T]: YamlDecoder[T1] = self.asInstanceOf[YamlDecoder[T1]]

  final def map[T1](f: T => T1): YamlDecoder[T1] = new YamlDecoder[T1] {
    override def construct(node: Node)(implicit
        settings: LoadSettings
    ): Either[ConstructError, T1] =
      self.construct(node).map(f)
  }

  final def mapError[T1](f: T => Either[ConstructError, T1]): YamlDecoder[T1] =
    new YamlDecoder[T1] {
      override def construct(node: Node)(implicit
          settings: LoadSettings
      ): Either[ConstructError, T1] =
        self.construct(node).flatMap(f)
    }

  final def flatMap[T1](f: T => YamlDecoder[T1]): YamlDecoder[T1] = new YamlDecoder[T1] {
    override def construct(node: Node)(implicit
        settings: LoadSettings
    ): Either[ConstructError, T1] =
      self.construct(node) match {
        case Right(result) => f(result).construct(node)
        case l @ Left(_)   => l.asInstanceOf[Left[ConstructError, Nothing]]
      }
  }
}

object YamlDecoder extends YamlDecoderCompanionCrossCompat {

  def apply[T](implicit self: YamlDecoder[T]): YamlDecoder[T] = self

  def from[T](pf: PartialFunction[Node, Either[ConstructError, T]])(implicit
      classTag: ClassTag[T]
  ): YamlDecoder[T] = apply[T](pf)

  def apply[T](
      pf: PartialFunction[Node, Either[ConstructError, T]]
  )(implicit classTag: ClassTag[T]): YamlDecoder[T] =
    new YamlDecoder[T] {
      override def construct(
          node: Node
      )(implicit settings: LoadSettings = LoadSettings.empty): Either[ConstructError, T] =
        if (node.tag == Tag.nullTag)
          Left(
            ConstructError.from(
              s"""|Could't construct ${classTag.runtimeClass.getName} from null (${node.tag.value})
                  |${node.pos.map(_.errorMsg).getOrElse("")}
                  |""".stripMargin
            )
          )
        else if (pf.isDefinedAt(node)) pf(node)
        else
          Left(
            ConstructError.from(
              s"""|Could't construct ${classTag.runtimeClass.getName} from ${node.tag.value}
                  |${node.pos.map(_.errorMsg).getOrElse("")}
                  |""".stripMargin
            )
          )
    }

  private def cannotParse(value: Any, tpe: String, node: Node) = ConstructError.from(
    s"Cannot parse $value as $tpe",
    node,
    tpe
  )

  private def normalizeInt(string: String): String = {
    val octal = if (string.startsWith("0o")) string.stripPrefix("0o").prepended('0') else string
    octal.replaceAll("_", "")
  }

  implicit def forInt: YamlDecoder[Int] = YamlDecoder { case s @ ScalarNode(value, _) =>
    Try(java.lang.Integer.decode(normalizeInt(value)).toInt).toEither.left
      .map(ConstructError.from(_, "Int", s))
  }

  implicit def forLong: YamlDecoder[Long] = YamlDecoder { case s @ ScalarNode(value, _) =>
    Try(java.lang.Long.decode(normalizeInt(value)).toLong).toEither.left
      .map(ConstructError.from(_, "Long", s))
  }

  implicit def forDouble: YamlDecoder[Double] = YamlDecoder { case s @ ScalarNode(value, _) =>
    if (Tag.nan.matches(value)) {
      Right(Double.NaN)
    } else if (Tag.plusInfinity.matches(value)) {
      Right(Double.PositiveInfinity)
    } else if (Tag.minusInfinity.matches(value)) {
      Right(Double.NegativeInfinity)
    } else {
      Try(java.lang.Double.parseDouble(value.replaceAll("_", ""))).toEither.left
        .map(ConstructError.from(_, "Double", s))
    }
  }

  def forDoublePrecise: YamlDecoder[Double] = YamlDecoder { case s @ ScalarNode(value, _) =>
    forDouble.construct(s).flatMap { n =>
      val ns = n.toString
      if (ns == value) Right(n) else Left(ConstructError.from(s"Double, decoded $ns", s))
    }
  }

  implicit def forFloat: YamlDecoder[Float] = YamlDecoder { case s @ ScalarNode(value, _) =>
    if (Tag.nan.matches(value)) {
      Right(Float.NaN)
    } else if (Tag.plusInfinity.matches(value)) {
      Right(Float.PositiveInfinity)
    } else if (Tag.minusInfinity.matches(value)) {
      Right(Float.NegativeInfinity)
    } else {
      Try(java.lang.Float.parseFloat(value.replaceAll("_", ""))).toEither.left
        .map(ConstructError.from(_, "Float", s))
    }
  }

  def forFloatPrecise: YamlDecoder[Float] = YamlDecoder { case s @ ScalarNode(value, _) =>
    forFloat.construct(s).flatMap { n =>
      val ns = n.toString
      if (ns == value) Right(n) else Left(ConstructError.from(s"Float, decoded $ns", s))
    }
  }

  implicit def forShort: YamlDecoder[Short] = YamlDecoder { case s @ ScalarNode(value, _) =>
    Try(java.lang.Short.decode(normalizeInt(value)).toShort).toEither.left
      .map(ConstructError.from(_, "Short", s))
  }

  implicit def forByte: YamlDecoder[Byte] = YamlDecoder { case s @ ScalarNode(value, _) =>
    Try(java.lang.Byte.decode(normalizeInt(value)).toByte).toEither.left
      .map(ConstructError.from(_, "Byte", s))
  }

  implicit def forBoolean: YamlDecoder[Boolean] = YamlDecoder { case s @ ScalarNode(value, _) =>
    if (Tag.falsePattern.matches(value)) {
      Right(false)
    } else if (Tag.truePattern.matches(value)) {
      Right(true)
    } else {
      Left(cannotParse(value, "Boolean", s))
    }
  }

  implicit def forBigInt: YamlDecoder[BigInt] = YamlDecoder { case s @ ScalarNode(value, _) =>
    Try(BigInt(normalizeInt(value))).toEither.left
      .map(ConstructError.from(_, "BigInt", s))
  }

  implicit def forBigDecimal: YamlDecoder[BigDecimal] = YamlDecoder {
    case s @ ScalarNode(value, _) =>
      Try(BigDecimal(value.replaceAll("_", ""))).toEither.left
        .map(ConstructError.from(_, "BigDecimal", s))
  }

  implicit def forAny: YamlDecoder[Any] = new YamlDecoder[Any] {
    def construct(node: Node)(implicit settings: LoadSettings = LoadSettings.empty) = node match {
      case ScalarNode(_, Tag.nullTag) =>
        Right(None)
      case node @ ScalarNode(_, Tag.boolean) =>
        forBoolean.construct(node)
      case node @ ScalarNode(_, Tag.int) =>
        forByte
          .widen[Any]
          .orElse(forShort.widen)
          .orElse(forInt.widen)
          .orElse(forLong.widen)
          .orElse(forBigInt.widen)
          .construct(node)
      case node @ ScalarNode(_, Tag.float) =>
        forFloatPrecise
          .widen[Any]
          .orElse(forDoublePrecise.widen)
          .orElse(forBigDecimal.widen)
          .construct(node)
      case ScalarNode(value, Tag.str) =>
        Right(value)
      case MappingNode(mappings, Tag.map) =>
        val decoder = implicitly[YamlDecoder[Map[Any, Any]]]
        decoder.construct(node)
      case SequenceNode(seq, Tag.seq) =>
        val decoder = implicitly[YamlDecoder[Seq[Any]]]
        decoder.construct(node)
      case _ =>
        settings.constructors.get(node.tag) match {
          case Some(decoder) => decoder.construct(node)
          case None =>
            Left(
              ConstructError.from(
                s"""|Could't construct runtime instance of ${node.tag}
                    |${node.pos.map(_.errorMsg).getOrElse("")}
                    |If you're using custom datatype consider using yaml.as[MyType] instead of Any
                    |Or define LoadSettings where you'll specify how to construct ${node.tag}
                    |""".stripMargin
              )
            )
        }
    }
  }

  implicit def forOption[T](implicit c: YamlDecoder[T]): YamlDecoder[Option[T]] =
    new YamlDecoder[Option[T]] {
      override def construct(
          node: Node
      )(implicit settings: LoadSettings): Either[ConstructError, Option[T]] =
        if (node.tag == Tag.nullTag) Right(None)
        else c.construct(node).map(Option(_))
    }

  private def constructFromNodes[T](nodes: Seq[Node])(implicit
      c: YamlDecoder[T]
  ): Either[ConstructError, Seq[T]] = {
    val constructed = nodes.map(c.construct(_))

    constructed.partitionMap(identity) match {
      case (Nil, rights) => Right(rights)
      case (lefts, _)    => Left(lefts.head)
    }
  }

  implicit def forList[T](implicit c: YamlDecoder[T]): YamlDecoder[List[T]] = YamlDecoder {
    case SequenceNode(nodes, _) =>
      constructFromNodes[T](nodes).map(_.toList)
  }

  implicit def forSeq[T](implicit c: YamlDecoder[T]): YamlDecoder[Seq[T]] = YamlDecoder {
    case SequenceNode(nodes, _) =>
      constructFromNodes[T](nodes)
  }

  implicit def forSet[T](implicit c: YamlDecoder[T]): YamlDecoder[Set[T]] = YamlDecoder {
    case SequenceNode(nodes, _) =>
      constructFromNodes[T](nodes).map(_.toSet)
  }

  implicit def forMap[K, V](implicit
      keyDecoder: YamlDecoder[K],
      valueDecoder: YamlDecoder[V]
  ): YamlDecoder[Map[K, V]] = YamlDecoder { case MappingNode(mappings, _) =>
    val decoded: Seq[
      Either[ConstructError, (K, V)]
    ] = mappings.toSeq
      .map { case (key, value) =>
        (keyDecoder.construct(key) -> valueDecoder.construct(value))
      }
      .map { case (key, value) =>
        for {
          k <- key
          v <- value
        } yield (k -> v)
      }

    decoded.partitionMap(identity) match {
      case (lefts, _) if lefts.nonEmpty => Left(lefts.head)
      case (_, rights)                  => Right(rights.toMap)
    }
  }

  implicit def forString: YamlDecoder[String] = YamlDecoder { case ScalarNode(value, _) =>
    Right(value)
  }

}
