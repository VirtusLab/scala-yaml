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
}

object YamlDecoder extends YamlDecoderCompanionCrossCompat {
  def apply[T](
      pf: PartialFunction[Node, Either[ConstructError, T]]
  )(implicit classTag: ClassTag[T]): YamlDecoder[T] =
    new YamlDecoder[T] {
      override def construct(
          node: Node
      )(implicit settings: LoadSettings = LoadSettings.empty): Either[ConstructError, T] =
        if (pf.isDefinedAt(node)) pf(node)
        else
          Left(
            ConstructError(s"""|Could't construct ${classTag.runtimeClass.getName} from ${node.tag}
                               |${node.pos.map(_.errorMsg).getOrElse("")}
                               |""".stripMargin)
          )
    }

  private def cannotParse(value: Any, tpe: String, node: Node) = ConstructError.from(
    s"Cannot parse $value as $tpe",
    node,
    tpe
  )

  implicit def forInt: YamlDecoder[Int] = YamlDecoder { case s @ ScalarNode(value, _) =>
    Try(java.lang.Integer.decode(value.replaceAll("_", "")).toInt).toEither.left
      .map(ConstructError.from(_, "Int", s))
  }

  implicit def forLong: YamlDecoder[Long] = YamlDecoder { case s @ ScalarNode(value, _) =>
    Try(java.lang.Long.decode(value.replaceAll("_", "")).toLong).toEither.left
      .map(ConstructError.from(_, "Long", s))
  }

  implicit def forDouble: YamlDecoder[Double] = YamlDecoder { case s @ ScalarNode(value, _) =>
    Try(java.lang.Double.parseDouble(value.replaceAll("_", ""))).toEither.left
      .map(ConstructError.from(_, "Double", s))
  }

  implicit def forFloat: YamlDecoder[Float] = YamlDecoder { case s @ ScalarNode(value, _) =>
    Try(java.lang.Float.parseFloat(value.replaceAll("_", ""))).toEither.left
      .map(ConstructError.from(_, "Float", s))
  }

  implicit def forShort: YamlDecoder[Short] = YamlDecoder { case s @ ScalarNode(value, _) =>
    Try(java.lang.Short.decode(value.replaceAll("_", "")).toShort).toEither.left
      .map(ConstructError.from(_, "Short", s))
  }

  implicit def forByte: YamlDecoder[Byte] = YamlDecoder { case s @ ScalarNode(value, _) =>
    Try(java.lang.Byte.decode(value.replaceAll("_", "")).toByte).toEither.left
      .map(ConstructError.from(_, "Byte", s))
  }

  implicit def forBoolean: YamlDecoder[Boolean] = YamlDecoder { case s @ ScalarNode(value, _) =>
    value.toBooleanOption.toRight(cannotParse(value, "Boolean", s))
  }

  implicit def forBigInt: YamlDecoder[BigInt] = YamlDecoder { case s @ ScalarNode(value, _) =>
    Try(BigInt(value.replaceAll("_", ""))).toEither.left
      .map(ConstructError.from(_, "BigInt", s))
  }

  implicit def forBigDecimal: YamlDecoder[BigDecimal] = YamlDecoder {
    case s @ ScalarNode(value, _) =>
      Try(BigDecimal(value.replaceAll("_", ""))).toEither.left
        .map(ConstructError.from(_, "BigDecimal", s))
  }

  implicit def forAny: YamlDecoder[Any] = new YamlDecoder[Any] {
    def construct(node: Node)(implicit settings: LoadSettings = LoadSettings.empty) = {
      node match {
        case node @ ScalarNode(value, tag: CoreSchemaTag) if Tag.corePrimitives.contains(tag) =>
          tag match {
            case Tag.nullTag =>
              Right(None)
            case Tag.boolean =>
              forBoolean.construct(node)
            case Tag.int =>
              forByte
                .widen[Any]
                .orElse(forShort.widen[Any])
                .orElse(forInt.widen[Any])
                .orElse(forLong.widen[Any])
                .orElse(forBigInt.widen[Any])
                .construct(node)
            case Tag.float =>
              forFloat
                .widen[Any]
                .orElse(forDouble.widen[Any])
                .orElse(forBigDecimal.widen[Any])
                .construct(node)
            case Tag.str =>
              Right(value)
          }
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
                ConstructError(
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
  }

  implicit def forOption[T](implicit c: YamlDecoder[T]): YamlDecoder[Option[T]] = YamlDecoder {
    node =>
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
