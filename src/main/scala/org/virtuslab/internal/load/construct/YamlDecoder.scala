package org.virtuslab.internal.load.construct

import org.virtuslab.internal.YamlError
import org.virtuslab.internal.ConstructError
import org.virtuslab.internal.load.compose.Node
import org.virtuslab.internal.load.compose.Node.*

import scala.compiletime._
import scala.compiletime.summonFrom
import scala.deriving._
import scala.util.Try

sealed trait YamlDecoder[T]:
  def construct(node: Node): Either[YamlError, T]

object YamlDecoder:
  def apply[T](pf: PartialFunction[Node, Either[Throwable, T]]): YamlDecoder[T] =
    new YamlDecoder[T] {
      override def construct(node: Node): Either[YamlError, T] =
        if pf.isDefinedAt(node) then
          pf(node) match
            case Right(value)    => Right(value)
            case Left(throwable) => Left(ConstructError(throwable.toString))
        else Left(ConstructError(s"Could't create Construct instance for $node"))
    }

  given YamlDecoder[Int] = YamlDecoder { case ScalarNode(value) =>
    Try(value.toInt).toEither
  }

  given YamlDecoder[Double] = YamlDecoder { case ScalarNode(value) =>
    Try(value.toDouble).toEither
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
      override def construct(node: Node): Either[YamlError, T] =
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
      override def construct(node: Node): Either[YamlError, T] = LazyList
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
