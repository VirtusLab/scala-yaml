package org.virtuslab.internal.load.construct

import org.virtuslab.internal.YamlError
import org.virtuslab.internal.load.compose.Node
import org.virtuslab.internal.load.compose.Node.*

import scala.compiletime._
import scala.compiletime.summonFrom
import scala.deriving._
import scala.util.Try

sealed trait Construct[T]:
  def construct(node: Node): Either[YamlError, T]

object Construct:
  def apply[T](pf: PartialFunction[Node, Either[Throwable, T]]): Construct[T] =
    new Construct[T] {
      override def construct(node: Node): Either[YamlError, T] =
        if pf.isDefinedAt(node) then
          pf(node) match
            case Right(value)    => Right(value)
            case Left(throwable) => Left(YamlError(throwable.toString))
        else Left(YamlError(s"Could't create Construct instance for $node"))
    }

  given Construct[Int] = Construct { case ScalarNode(value) =>
    Try(value.toInt).toEither
  }

  given Construct[Double] = Construct { case ScalarNode(value) =>
    Try(value.toDouble).toEither
  }

  given Construct[String] = Construct { case ScalarNode(value) =>
    Right(value)
  }

  inline given derived[T](using m: Mirror.Of[T]): Construct[T] = inline m match
    case p: Mirror.ProductOf[T] => product(p)
    case s: Mirror.SumOf[T]     => sumOf(s)

  inline def product[T](p: Mirror.ProductOf[T]) =
    val instances  = summonAll[p.MirroredElemTypes]
    val elemLabels = getElemLabels[p.MirroredElemLabels]
    new Construct[T] {
      override def construct(node: Node): Either[YamlError, T] =
        node match
          case Node.MappingNode(mappings) =>
            val valuesMap = mappings.map(e => e.key.value -> e.value).toMap
            val values = elemLabels.zip(instances).map { case (label, c) =>
              valuesMap.get(label) match
                case Some(value) => c.construct(value)
                case None        => Left(YamlError(s"Key $label doesn't exist in parsed document"))
            }
            val (left, right) = values.partitionMap(identity)
            if left.nonEmpty then Left(left.head)
            else Right(p.fromProduct(Tuple.fromArray(right.toArray)))
          case _ => Left(YamlError(s"Expected MappingNode, got ${node.getClass.getSimpleName}"))
    }

  inline def sumOf[T](s: Mirror.SumOf[T]) =
    val instances = summonAll[s.MirroredElemTypes].asInstanceOf[List[Construct[T]]]
    new Construct[T]:
      override def construct(node: Node): Either[YamlError, T] = LazyList
        .from(instances)
        .map(c => c.construct(node))
        .collectFirst { case r @ Right(_) => r }
        .getOrElse(Left(YamlError(s"Cannot parse $node")))

  inline def summonAll[T <: Tuple]: List[Construct[_]] = inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => summonInline[Construct[t]] :: summonAll[ts]

  inline def getElemLabels[T <: Tuple]: List[String] = inline erasedValue[T] match
    case _: EmptyTuple     => Nil
    case _: (head *: tail) => constValue[head].toString :: getElemLabels[tail]
