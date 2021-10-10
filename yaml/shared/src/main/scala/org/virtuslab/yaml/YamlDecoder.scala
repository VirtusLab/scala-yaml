package org.virtuslab.yaml

import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Node.*

import scala.compiletime.*
import scala.deriving.*
import scala.util.Try
import scala.reflect.ClassTag

/**
 * A type class that provides a conversion from a [[Node]] into given type [[T]]
 */
trait YamlDecoder[T]:
  def construct(node: Node): Either[ConstructError, T]

object YamlDecoder:
  def apply[T](
      pf: PartialFunction[Node, Either[ConstructError | Throwable, T]]
  )(implicit tag: ClassTag[T]): YamlDecoder[T] =
    new YamlDecoder[T] {
      override def construct(node: Node): Either[ConstructError, T] =
        if pf.isDefinedAt(node) then
          pf(node) match {
            case Left(e: Throwable) =>
              val msg = node.pos match {
                case Some(pos) =>
                  s"""|${e.getMessage}
                      |at ${pos.line}:${pos.column}, expected $tag
                      |$pos """.stripMargin
                case None =>
                  "Cannot decode: ${e.getMessage}"
              }
              Left(ConstructError(msg))
            case Left(e: ConstructError) => Left(e)
            case Right(v)                => Right(v)
          }
        else Left(ConstructError(s"Could't create Construct instance for $node"))
    }

  given YamlDecoder[Int] = YamlDecoder { case ScalarNode(value, _) =>
    Try(value.toInt).toEither
  }

  given YamlDecoder[Long] = YamlDecoder { case ScalarNode(value, _) =>
    Try(value.toLong).toEither
  }

  given YamlDecoder[Double] = YamlDecoder { case ScalarNode(value, _) =>
    Try(value.toDouble).toEither
  }

  given YamlDecoder[Float] = YamlDecoder { case ScalarNode(value, _) =>
    Try(value.toFloat).toEither
  }

  given YamlDecoder[Short] = YamlDecoder { case ScalarNode(value, _) =>
    Try(value.toShort).toEither
  }

  given YamlDecoder[Byte] = YamlDecoder { case ScalarNode(value, _) =>
    Try(value.toByte).toEither
  }

  given YamlDecoder[Boolean] = YamlDecoder { case ScalarNode(value, _) =>
    Try(value.toBoolean).toEither
  }

  given [T](using c: YamlDecoder[T]): YamlDecoder[Option[T]] = YamlDecoder {
    case ScalarNode(value, _) =>
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
    case SequenceNode(nodes, _) =>
      constructFromNodes(nodes).map(_.toList)
  }

  given [T](using c: YamlDecoder[T]): YamlDecoder[Seq[T]] = YamlDecoder {
    case SequenceNode(nodes, _) =>
      constructFromNodes(nodes)
  }

  given [T](using c: YamlDecoder[T]): YamlDecoder[Set[T]] = YamlDecoder {
    case SequenceNode(nodes, _) =>
      constructFromNodes(nodes).map(_.toSet)
  }

  given [K, V](using
      keyDecoder: YamlDecoder[K],
      valueDecoder: YamlDecoder[V]
  ): YamlDecoder[Map[K, V]] = YamlDecoder { case MappingNode(mappings, _) =>
    val decoded: Seq[
      Either[ConstructError, (K, V)]
    ] = mappings
      .map { case KeyValueNode(key, value, _) =>
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

  given YamlDecoder[String] = YamlDecoder { case ScalarNode(value, _) =>
    Right(value)
  }

  inline def derived[T](using m: Mirror.Of[T]): YamlDecoder[T] = inline m match
    case p: Mirror.ProductOf[T] => deriveProduct(p)
    case s: Mirror.SumOf[T]     => sumOf(s)

  private def extractKeyValues(
      mappings: Seq[KeyValueNode]
  ): Either[ConstructError, Map[String, Node]] = {
    val (error, valuesSeq) = mappings
      .map { mapping =>
        val key = mapping.key match {
          case ScalarNode(k, _) => Right(k)
          case _ => Left(ConstructError(s"Parameter of a class must be a scalar value"))
        }
        val value = mapping.value
        key.map(k => k -> value)
      }
      .partitionMap(identity)

    if (error.nonEmpty) Left(error.head)
    else Right(valuesSeq.toMap)
  }

  private def constructValues[T](
      elemLabels: List[String],
      instances: List[YamlDecoder[_]],
      valuesMap: Map[String, Node],
      p: Mirror.ProductOf[T]
  ) = {
    val values = elemLabels.zip(instances).map { case (label, c) =>
      valuesMap.get(label) match
        case Some(value) => c.construct(value)
        case None        => Left(ConstructError(s"Key $label doesn't exist in parsed document"))
    }
    val (left, right) = values.partitionMap(identity)
    if left.nonEmpty then Left(left.head)
    else Right(p.fromProduct(Tuple.fromArray(right.toArray)))
  }

  private inline def deriveProduct[T](p: Mirror.ProductOf[T]) =
    val instances  = summonAll[p.MirroredElemTypes]
    val elemLabels = getElemLabels[p.MirroredElemLabels]
    new YamlDecoder[T] {
      override def construct(node: Node): Either[ConstructError, T] =
        node match
          case Node.MappingNode(mappings, _) =>
            for {
              valuesMap        <- extractKeyValues(mappings)
              construcedValues <- constructValues(elemLabels, instances, valuesMap, p)
            } yield (construcedValues)
          case _ =>
            Left(ConstructError(s"Expected MappingNode, got ${node.getClass.getSimpleName}"))
    }

  private inline def sumOf[T](s: Mirror.SumOf[T]) =
    val instances = summonSumOf[s.MirroredElemTypes].asInstanceOf[List[YamlDecoder[T]]]
    new YamlDecoder[T]:
      override def construct(node: Node): Either[ConstructError, T] = LazyList
        .from(instances)
        .map(c => c.construct(node))
        .collectFirst { case r @ Right(_) => r }
        .getOrElse(Left(ConstructError(s"Cannot parse $node")))

  private inline def summonSumOf[T <: Tuple]: List[YamlDecoder[_]] = inline erasedValue[T] match
    case _: (t *: ts) =>
      summonFrom { case p: Mirror.ProductOf[`t`] =>
        deriveProduct(p) :: summonSumOf[ts]
      }
    case _: EmptyTuple => Nil

  private inline def summonAll[T <: Tuple]: List[YamlDecoder[_]] = inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => summonInline[YamlDecoder[t]] :: summonAll[ts]

  private inline def getElemLabels[T <: Tuple]: List[String] = inline erasedValue[T] match
    case _: EmptyTuple     => Nil
    case _: (head *: tail) => constValue[head].toString :: getElemLabels[tail]
