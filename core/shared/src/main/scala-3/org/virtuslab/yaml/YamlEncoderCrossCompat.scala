package org.virtuslab.yaml

import scala.deriving.Mirror
import scala.compiletime.*

private[yaml] trait YamlEncoderCrossCompanionCompat {
  inline def derived[T](using m: Mirror.Of[T]): YamlEncoder[T] = inline m match
    case p: Mirror.ProductOf[T] => deriveProduct(p)
    case s: Mirror.SumOf[T]     => deriveSum(s)

  private inline def deriveProduct[T](p: Mirror.ProductOf[T]): YamlEncoder[T] =
    new YamlEncoder[T] {
      val yamlEncoders = summonAll[p.MirroredElemTypes]
      val elemLabels   = getElemLabels[p.MirroredElemLabels]
      override def asNode(obj: T): Node =
        val products = obj.asInstanceOf[Product].productIterator
        val nodes =
          elemLabels
            .zip(products)
            .zip(yamlEncoders)
            .map { case ((label, element), encoder) =>
              val key: Node = Node.ScalarNode(label)
              val value     = encoder.asInstanceOf[YamlEncoder[Any]].asNode(element)
              (key, value)
            }
            .toMap
        Node.MappingNode(nodes)
    }

  private inline def deriveSum[T](s: Mirror.SumOf[T]) =
    new YamlEncoder[T]:
      val yamlEncoders = summonSumOf[s.MirroredElemTypes].asInstanceOf[List[YamlEncoder[T]]]
      override def asNode(t: T): Node =
        val index = s.ordinal(t)
        yamlEncoders(index).asInstanceOf[YamlEncoder[Any]].asNode(t)

  private inline def summonSumOf[T <: Tuple]: List[YamlEncoder[_]] = inline erasedValue[T] match
    case _: (t *: ts) =>
      summonFrom { case p: Mirror.ProductOf[`t`] =>
        deriveProduct(p) :: summonSumOf[ts]
      }
    case _: EmptyTuple => Nil

  private inline def summonAll[T <: Tuple]: List[YamlEncoder[_]] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => summonInline[YamlEncoder[t]] :: summonAll[ts]
  }

  private inline def getElemLabels[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: EmptyTuple     => Nil
    case _: (head *: tail) => constValue[head].toString :: getElemLabels[tail]
  }
}
