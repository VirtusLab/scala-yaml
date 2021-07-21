package org.virtuslab.internal.dump

import scala.deriving._
import scala.compiletime._
import scala.deriving.Mirror
import org.virtuslab.internal.load.compose.Node

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

  given [T](using encoder: YamlEncoder[T]): YamlEncoder[Set[T]] = (nodes) =>
    Node.SequenceNode(nodes.map(encoder.asNode(_)).toSeq)

  given [T](using encoder: YamlEncoder[T]): YamlEncoder[Seq[T]] = (nodes) =>
    Node.SequenceNode(nodes.map(encoder.asNode(_)))

  given [T](using encoder: YamlEncoder[T]): YamlEncoder[List[T]] = (nodes) =>
    Node.SequenceNode(nodes.map(encoder.asNode(_)))

  // todo support arbitrary node as key in KeyValueNode
  given [V](using valueCodec: YamlEncoder[V]): YamlEncoder[Map[String, V]] =
    (nodes) =>
      val pairs = nodes.toList.map((key, value) =>
        Node.KeyValueNode(Node.ScalarNode(key), valueCodec.asNode(value))
      )
      Node.MappingNode(pairs)

  inline given derived[T](using m: Mirror.Of[T]): YamlEncoder[T] =
    val yamlEncoders = summonAll[m.MirroredElemTypes]
    inline m match
      case p: Mirror.ProductOf[T] => productOf(p, yamlEncoders)
      case s: Mirror.SumOf[T]     => sumOf(s, yamlEncoders)

  inline def productOf[T](
      p: Mirror.ProductOf[T],
      yamlEncoders: List[YamlEncoder[_]]
  ): YamlEncoder[T] =
    new YamlEncoder[T] {
      override def asNode(obj: T): Node =
        val products   = obj.asInstanceOf[Product].productIterator
        val elemLabels = getElemLabels[p.MirroredElemLabels]
        val nodes =
          elemLabels.zip(products).zip(yamlEncoders).map { case ((label, element), encoder) =>
            val key   = Node.ScalarNode(label)
            val value = encoder.asInstanceOf[YamlEncoder[Any]].asNode(element)
            Node.KeyValueNode(key, value)
          }
        Node.MappingNode(nodes)
    }

  inline def sumOf[T](s: Mirror.SumOf[T], yamlEncoders: List[YamlEncoder[_]]) =
    new YamlEncoder[T]:
      override def asNode(t: T): Node =
        val index = s.ordinal(t)
        yamlEncoders(index).asInstanceOf[YamlEncoder[Any]].asNode(t)

  inline def summonAll[T <: Tuple]: List[YamlEncoder[_]] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => summonInline[YamlEncoder[t]] :: summonAll[ts]
  }

  inline def getElemLabels[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: EmptyTuple     => Nil
    case _: (head *: tail) => constValue[head].toString :: getElemLabels[tail]
  }
