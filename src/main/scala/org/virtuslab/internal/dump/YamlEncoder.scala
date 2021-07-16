package org.virtuslab.internal.dump

import scala.deriving._
import scala.compiletime._
import scala.deriving.Mirror

trait YamlEncoder[T]:
  def toYaml(obj: T): String

object YamlEncoder:
  extension [T: YamlEncoder](t: T) def toYaml(): String = summon[YamlEncoder[T]].toYaml(t)

  given YamlEncoder[Int] = _.toString

  given YamlEncoder[String] = _.toString

  given YamlEncoder[Double] = _.toString

  given seqWriter[T](using writer: YamlEncoder[T]): YamlEncoder[Seq[T]] with
    override def toYaml(elements: Seq[T]): String = {
      val values = elements.map(writer.toYaml(_))
      values.mkString("- ", "\n- ", "\n")
    }

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
      override def toYaml(obj: T): String =
        val products   = obj.asInstanceOf[Product].productIterator
        val elemLabels = getElemLabels[p.MirroredElemLabels]

        val values =
          elemLabels.zip(products).zip(yamlEncoders).map { case ((label, value), yamlWriter) =>
            s"$label: ${yamlWriter.asInstanceOf[YamlEncoder[Any]].toYaml(value)}"
          }
        values.mkString("\n")
    }

  inline def sumOf[T](s: Mirror.SumOf[T], yamlEncoders: List[YamlEncoder[_]]) =
    new YamlEncoder[T]:
      override def toYaml(t: T): String =
        val index = s.ordinal(t)
        yamlEncoders(index).asInstanceOf[YamlEncoder[Any]].toYaml(t)

  inline def summonAll[T <: Tuple]: List[YamlEncoder[_]] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => summonInline[YamlEncoder[t]] :: summonAll[ts]
  }

  inline def getElemLabels[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: EmptyTuple     => Nil
    case _: (head *: tail) => constValue[head].toString :: getElemLabels[tail]
  }
