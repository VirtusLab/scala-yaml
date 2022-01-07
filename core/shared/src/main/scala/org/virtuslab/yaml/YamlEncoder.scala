package org.virtuslab.yaml

import scala.compiletime._
import scala.deriving.Mirror

/**
 * A type class that provides a conversion from a given type [[T]] into [[Node]]
 */
trait YamlEncoder[T]:
  def asNode(obj: T): Node

object YamlEncoder:
  given YamlEncoder[Byte]    = v => Node.ScalarNode(v.toString)
  given YamlEncoder[Char]    = v => Node.ScalarNode(v.toString)
  given YamlEncoder[Short]   = v => Node.ScalarNode(v.toString)
  given YamlEncoder[Int]     = v => Node.ScalarNode(v.toString)
  given YamlEncoder[Long]    = v => Node.ScalarNode(v.toString)
  given YamlEncoder[Float]   = v => Node.ScalarNode(v.toString)
  given YamlEncoder[Double]  = v => Node.ScalarNode(v.toString)
  given YamlEncoder[Boolean] = v => Node.ScalarNode(v.toString)
  given YamlEncoder[String]  = v => Node.ScalarNode(v)

  given [T](using encoder: YamlEncoder[T]): YamlEncoder[Option[T]] = {
    case Some(t) => encoder.asNode(t)
    case None => Node.ScalarNode("", Tag.nullTag)
  }

  given [T](using encoder: YamlEncoder[T]): YamlEncoder[Set[T]] = (nodes) =>
    Node.SequenceNode(nodes.map(encoder.asNode(_)).toSeq, Tag.seq)

  given [T](using encoder: YamlEncoder[T]): YamlEncoder[Seq[T]] = (nodes) =>
    Node.SequenceNode(nodes.map(encoder.asNode(_)), Tag.seq)

  given [T](using encoder: YamlEncoder[T]): YamlEncoder[List[T]] = (nodes) =>
    Node.SequenceNode(nodes.map(encoder.asNode(_)), Tag.seq)

  // todo support arbitrary node as key in KeyValueNode
  given [K, V](using keyCodec: YamlEncoder[K], valueCodec: YamlEncoder[V]): YamlEncoder[Map[K, V]] =
    (nodes) =>
      val mappings = nodes.map((key, value) => (keyCodec.asNode(key) -> valueCodec.asNode(value)))
      Node.MappingNode(mappings)

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
