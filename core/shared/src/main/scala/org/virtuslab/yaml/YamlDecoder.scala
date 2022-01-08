package org.virtuslab.yaml

import scala.compiletime.*
import scala.deriving.*
import scala.reflect.ClassTag
import scala.util.Try

import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Node.*

/**
 * A type class that provides a conversion from a [[Node]] into given type [[T]]
 */
trait YamlDecoder[T]:
  def construct(node: Node)(using
      settings: LoadSettings = LoadSettings.empty
  ): Either[ConstructError, T]

object YamlDecoder:
  def apply[T](
      pf: PartialFunction[Node, Either[ConstructError, T]]
  )(using classTag: ClassTag[T]): YamlDecoder[T] =
    new YamlDecoder[T] {
      override def construct(
          node: Node
      )(using settings: LoadSettings = LoadSettings.empty): Either[ConstructError, T] =
        if pf.isDefinedAt(node) then pf(node)
        else
          Left(
            ConstructError(s"""|Could't construct ${classTag.runtimeClass.getName} from ${node.tag}
                               |${node.pos.map(_.errorMsg).getOrElse("")}
                               |""".stripMargin)
          )
    }

  private inline def cannotParse(value: Any, tpe: String, node: Node) = ConstructError.from(
    s"Cannot parse $value as $tpe",
    node,
    tpe
  )

  given YamlDecoder[Int] = YamlDecoder { case s @ ScalarNode(value, _) =>
    value.toIntOption.toRight(cannotParse(value, "Int", s))
  }

  given YamlDecoder[Long] = YamlDecoder { case s @ ScalarNode(value, _) =>
    value.toLongOption.toRight(cannotParse(value, "Long", s))
  }

  given YamlDecoder[Double] = YamlDecoder { case s @ ScalarNode(value, _) =>
    value.toDoubleOption.toRight(cannotParse(value, "Double", s))
  }

  given YamlDecoder[Float] = YamlDecoder { case s @ ScalarNode(value, _) =>
    value.toFloatOption.toRight(cannotParse(value, "Float", s))
  }

  given YamlDecoder[Short] = YamlDecoder { case s @ ScalarNode(value, _) =>
    value.toShortOption.toRight(cannotParse(value, "Short", s))
  }

  given YamlDecoder[Byte] = YamlDecoder { case s @ ScalarNode(value, _) =>
    value.toByteOption.toRight(cannotParse(value, "Byte", s))
  }

  given YamlDecoder[Boolean] = YamlDecoder { case s @ ScalarNode(value, _) =>
    value.toBooleanOption.toRight(cannotParse(value, "Boolean", s))
  }

  given YamlDecoder[Any] = new YamlDecoder {
    def construct(node: Node)(using settings: LoadSettings = LoadSettings.empty) = {
      node match {
        case ScalarNode(value, tag: CoreSchemaTag) if Tag.corePrimitives.contains(tag) =>
          tag match {
            case Tag.nullTag => Right(None)
            case Tag.boolean => value.toBooleanOption.toRight(cannotParse(value, "Boolean", node))
            case Tag.int =>
              if value.startsWith("0b") then
                Try(Integer.parseInt(value.drop(2), 8)).toEither.left
                  .map(t => ConstructError.from(t, "Int", node))
              else if value.startsWith("0x") then
                Try(Integer.parseInt(value.drop(2), 8)).toEither.left
                  .map(t => ConstructError.from(t, "Int", node))
              else value.toIntOption.toRight(cannotParse(value, "Int", node))
            case Tag.float =>
              value.toDoubleOption.toRight(cannotParse(value, "Double", node))
            case Tag.str => Right(value)
          }
        case MappingNode(mappings, Tag.map) =>
          val decoder = summon[YamlDecoder[Map[Any, Any]]]
          decoder.construct(node)
        case SequenceNode(seq, Tag.seq) =>
          val decoder = summon[YamlDecoder[Seq[Any]]]
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

  given [T](using c: YamlDecoder[T]): YamlDecoder[Option[T]] = YamlDecoder { node =>
    if node.tag == Tag.nullTag then Right(None)
    else c.construct(node).map(Option(_))
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
    ] = mappings.toSeq
      .map { (key, value) =>
        (keyDecoder.construct(key) -> valueDecoder.construct(value))
      }
      .map { case (key, value) =>
        for
          k <- key
          v <- value
        yield (k -> v)
      }

    decoded.partitionMap(identity) match
      case (lefts, _) if lefts.nonEmpty => Left(lefts.head)
      case (_, rights)                  => Right(rights.toMap)
  }

  given YamlDecoder[String] = YamlDecoder { case ScalarNode(value, _) =>
    Right(value)
  }

  inline def derived[T](using m: Mirror.Of[T]): YamlDecoder[T] = inline m match
    case p: Mirror.ProductOf[T] => deriveProduct(p)
    case s: Mirror.SumOf[T]     => sumOf(s)

  private def extractKeyValues(
      mappings: Map[Node, Node]
  ): Either[ConstructError, Map[String, Node]] = {
    val keyValueMap = mappings
      .map { (k, v) =>
        k match {
          case ScalarNode(scalarKey, _) => Right((scalarKey, v))
          case _ => Left(ConstructError(s"Parameter of a class must be a scalar value"))
        }
      }
    val (error, valuesSeq) = keyValueMap.partitionMap(identity)

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
      override def construct(node: Node)(using
          constructor: LoadSettings = LoadSettings.empty
      ): Either[ConstructError, T] =
        node match
          case Node.MappingNode(mappings, _) =>
            for {
              valuesMap         <- extractKeyValues(mappings)
              constructedValues <- constructValues(elemLabels, instances, valuesMap, p)
            } yield (constructedValues)
          case _ =>
            Left(ConstructError(s"Expected MappingNode, got ${node.getClass.getSimpleName}"))
    }

  private inline def sumOf[T](s: Mirror.SumOf[T]) =
    val instances = summonSumOf[s.MirroredElemTypes].asInstanceOf[List[YamlDecoder[T]]]
    new YamlDecoder[T]:
      override def construct(
          node: Node
      )(using constructor: LoadSettings = LoadSettings.empty): Either[ConstructError, T] = LazyList
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
