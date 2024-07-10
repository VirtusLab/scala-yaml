package org.virtuslab.yaml

import org.virtuslab.yaml.Node.*

import scala.compiletime.*
import scala.deriving.Mirror

private[yaml] trait YamlDecoderCompanionCrossCompat extends DecoderMacros {
  inline def derived[T](using m: Mirror.Of[T]): YamlDecoder[T] = inline m match
    case p: Mirror.ProductOf[T] => deriveProduct(p)
    case s: Mirror.SumOf[T]     => sumOf(s)
}

private[yaml] trait DecoderMacros {
  protected def extractKeyValues(
      mappings: Map[Node, Node]
  ): Either[ConstructError, Map[String, Node]] = {
    val keyValueMap = mappings
      .map { (k, v) =>
        k match {
          case ScalarNode(scalarKey, _) => Right((scalarKey, v))
          case node =>
            Left(ConstructError.from(s"Parameter of a class must be a scalar value", node))
        }
      }
    val (error, valuesSeq) = keyValueMap.partitionMap(identity)

    if (error.nonEmpty) Left(error.head)
    else Right(valuesSeq.toMap)
  }

  protected def constructValues[T](
      elemLabels: List[String],
      instances: List[YamlDecoder[_]],
      optionalTypes: List[Boolean],
      valuesMap: Map[String, Node],
      p: Mirror.ProductOf[T],
      parentNode: Node
  ) = {
    val values = elemLabels.zip(instances).zip(optionalTypes).map { case ((label, c), isOptional) =>
      valuesMap.get(label) match
        case Some(value) => c.construct(value)
        case None =>
          if (isOptional) Right(None)
          else Left(ConstructError.from(s"Key $label doesn't exist in parsed document", parentNode))
    }
    val (left, right) = values.partitionMap(identity)
    if left.nonEmpty then Left(left.head)
    else Right(p.fromProduct(Tuple.fromArray(right.toArray)))
  }

  protected inline def deriveProduct[T](p: Mirror.ProductOf[T]) =
    val instances     = summonAll[p.MirroredElemTypes]
    val elemLabels    = getElemLabels[p.MirroredElemLabels]
    val optionalTypes = getOptionalTypes[p.MirroredElemTypes]
    new YamlDecoder[T] {
      override def construct(node: Node)(using
          constructor: LoadSettings = LoadSettings.empty
      ): Either[ConstructError, T] =
        node match
          case Node.MappingNode(mappings, _) =>
            for {
              valuesMap <- extractKeyValues(mappings)
              constructedValues <- constructValues(
                elemLabels,
                instances,
                optionalTypes,
                valuesMap,
                p,
                node
              )
            } yield (constructedValues)
          case _ =>
            Left(
              ConstructError.from(s"Expected MappingNode, got ${node.getClass.getSimpleName}", node)
            )
    }

  protected inline def sumOf[T](s: Mirror.SumOf[T]) =
    val instances = summonSumOf[s.MirroredElemTypes].asInstanceOf[List[YamlDecoder[T]]]
    new YamlDecoder[T]:
      override def construct(
          node: Node
      )(using constructor: LoadSettings = LoadSettings.empty): Either[ConstructError, T] = LazyList
        .from(instances)
        .map(c => c.construct(node))
        .collectFirst { case r @ Right(_) => r }
        .getOrElse(Left(ConstructError.from(s"Cannot parse $node", node)))

  protected inline def summonSumOf[T <: Tuple]: List[YamlDecoder[_]] = inline erasedValue[T] match
    case _: (t *: ts) =>
      summonFrom { case p: Mirror.ProductOf[`t`] =>
        deriveProduct(p) :: summonSumOf[ts]
      }
    case _: EmptyTuple => Nil

  protected inline def summonAll[T <: Tuple]: List[YamlDecoder[_]] = inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => summonInline[YamlDecoder[t]] :: summonAll[ts]

  protected inline def getElemLabels[T <: Tuple]: List[String] = inline erasedValue[T] match
    case _: EmptyTuple     => Nil
    case _: (head *: tail) => constValue[head].toString :: getElemLabels[tail]

  protected inline def getOptionalTypes[T <: Tuple]: List[Boolean] = inline erasedValue[T] match
    case _: EmptyTuple          => Nil
    case _: (Option[_] *: tail) => true :: getOptionalTypes[tail]
    case _: (_ *: tail)         => false :: getOptionalTypes[tail]

}
