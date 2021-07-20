package org.virtuslab.internal.load.construct

import org.virtuslab.internal.ConstructError
import org.virtuslab.internal.load.compose.Node
import org.virtuslab.internal.load.compose.Node.*

import scala.compiletime._
import scala.compiletime.summonFrom
import scala.deriving._
import scala.util.Try
import scala.language.implicitConversions

sealed trait YamlDecoder[T]:
  def construct(node: Node): Either[ConstructError, T]

object YamlDecoder:
  def apply[T](pf: PartialFunction[Node, Either[ConstructError, T]]): YamlDecoder[T] =
    new YamlDecoder[T] {
      override def construct(node: Node): Either[ConstructError, T] =
        if pf.isDefinedAt(node) then pf(node)
        else Left(ConstructError(s"Could't create Construct instance for $node"))
    }

  given throwableToConstructError[T]: Conversion[Either[Throwable, T], Either[ConstructError, T]] =
    _ match
      case Left(e) =>
        Left(ConstructError(e.getMessage))
      case Right(v) => Right(v)

  given YamlDecoder[Int] = YamlDecoder { case ScalarNode(value) =>
    Try(value.toInt).toEither
  }

  given YamlDecoder[Long] = YamlDecoder { case ScalarNode(value) =>
    Try(value.toLong).toEither
  }

  given YamlDecoder[Double] = YamlDecoder { case ScalarNode(value) =>
    Try(value.toDouble).toEither
  }

  given YamlDecoder[Float] = YamlDecoder { case ScalarNode(value) =>
    Try(value.toFloat).toEither
  }

  given YamlDecoder[Short] = YamlDecoder { case ScalarNode(value) =>
    Try(value.toShort).toEither
  }

  given YamlDecoder[Byte] = YamlDecoder { case ScalarNode(value) =>
    Try(value.toByte).toEither
  }

  given YamlDecoder[Boolean] = YamlDecoder { case ScalarNode(value) =>
    Try(value.toBoolean).toEither
  }

  given [T](using c: YamlDecoder[T]): YamlDecoder[Option[T]] = YamlDecoder {
    case ScalarNode(value) =>
      value match
        case "null" | "" => Right(None)
        case _ =>
          c.construct(ScalarNode(value)).map(Option(_))
  }

  private def constructFromNodes[T](nodes: Seq[Node])(using
      c: YamlDecoder[T]
  ): Either[ConstructError, Seq[T]] =
    val constructed = nodes.map(c.construct(_))

    constructed.partitionMap(identity) match
      case (Nil, rights) => Right(rights)
      case (lefts, _)    => Left(lefts.head)

  given [T](using c: YamlDecoder[T]): YamlDecoder[List[T]] = YamlDecoder {
    case SequenceNode(nodes) =>
      constructFromNodes(nodes).map(_.toList)
  }

  given [T](using c: YamlDecoder[T]): YamlDecoder[Seq[T]] = YamlDecoder {
    case SequenceNode(nodes) =>
      constructFromNodes(nodes)
  }

  given [T](using c: YamlDecoder[T]): YamlDecoder[Set[T]] = YamlDecoder {
    case SequenceNode(nodes) =>
      constructFromNodes(nodes).map(_.toSet)
  }

  given [K, V](using
      keyDecoder: YamlDecoder[K],
      valueDecoder: YamlDecoder[V]
  ): YamlDecoder[Map[K, V]] = YamlDecoder { case MappingNode(mappings) =>
    val decoded: Seq[
      Either[ConstructError, (K, V)]
    ] = mappings
      .map { case KeyValueNode(key, value) =>
        (keyDecoder.construct(key) -> valueDecoder.construct(value))
      }
      .map { case (key, value) =>
        for
          k <- key
          v <- value
        yield (k -> v)
      }

    decoded.partitionMap(identity) match
      case (Nil, rights) => Right(rights.toMap)
      case (lefts, _)    => Left(lefts.head)
  }

  given YamlDecoder[String] = YamlDecoder { case ScalarNode(value) =>
    Right(value)
  }

  inline given derived[T](using m: Mirror.Of[T]): YamlDecoder[T] = inline m match
    case p: Mirror.ProductOf[T] => product(p)
    case s: Mirror.SumOf[T]     => sumOf(s)

  private inline def product[T](p: Mirror.ProductOf[T]) =
    val instances  = summonAll[p.MirroredElemTypes]
    val elemLabels = getElemLabels[p.MirroredElemLabels]
    new YamlDecoder[T] {
      override def construct(node: Node): Either[ConstructError, T] =
        node match
          case Node.MappingNode(mappings) =>
            val valuesMap = mappings.map(e => e.key.value -> e.value).toMap
            val values = elemLabels.zip(instances).map { case (label, c) =>
              valuesMap.get(label) match
                case Some(value) => c.construct(value)
                case None => Left(ConstructError(s"Key $label doesn't exist in parsed document"))
            }
            val (left, right) = values.partitionMap(identity)
            if left.nonEmpty then Left(left.head)
            else Right(p.fromProduct(Tuple.fromArray(right.toArray)))
          case _ =>
            Left(ConstructError(s"Expected MappingNode, got ${node.getClass.getSimpleName}"))
    }

  private inline def sumOf[T](s: Mirror.SumOf[T]) =
    val instances = summonAll[s.MirroredElemTypes].asInstanceOf[List[YamlDecoder[T]]]
    new YamlDecoder[T]:
      override def construct(node: Node): Either[ConstructError, T] = LazyList
        .from(instances)
        .map(c => c.construct(node))
        .collectFirst { case r @ Right(_) => r }
        .getOrElse(Left(ConstructError(s"Cannot parse $node")))

  private inline def summonAll[T <: Tuple]: List[YamlDecoder[_]] = inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => summonInline[YamlDecoder[t]] :: summonAll[ts]

  private inline def getElemLabels[T <: Tuple]: List[String] = inline erasedValue[T] match
    case _: EmptyTuple     => Nil
    case _: (head *: tail) => constValue[head].toString :: getElemLabels[tail]
