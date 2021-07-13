package org.virtuslab.internal.dump

import scala.deriving._
import scala.compiletime._
import scala.deriving.Mirror

trait YamlWriter[T]:
  def toYaml(obj: T): String

object YamlWriter:
  extension [T: YamlWriter](t: T) def toYaml(): String = summon[YamlWriter[T]].toYaml(t)

  given YamlWriter[Int] with
    override def toYaml(obj: Int): String = obj.toString

  given YamlWriter[String] with
    override def toYaml(obj: String): String = obj.toString

  given YamlWriter[Double] with
    override def toYaml(obj: Double): String = obj.toString

  given seqWriter[T](using writer: YamlWriter[T]): YamlWriter[Seq[T]] with
    override def toYaml(elements: Seq[T]): String = {
      val values = elements.map(writer.toYaml(_))
      values.mkString("- ", "\n- ", "\n")
    }

  inline given derived[T](using m: Mirror.Of[T]): YamlWriter[T] =
    val yamlEncoders = summonAll[m.MirroredElemTypes]
    inline m match
      case p: Mirror.ProductOf[T] => productOf(yamlEncoders)
      case s: Mirror.SumOf[T]     => sumOf(s, yamlEncoders)

  inline def productOf[T](
      yamlEncoders: List[YamlWriter[_]]
  )(using m: Mirror.Of[T]): YamlWriter[T] =
    new YamlWriter[T] {
      override def toYaml(obj: T): String =
        val products   = obj.asInstanceOf[Product].productIterator
        val elemLabels = getElemLabels[m.MirroredElemLabels]

        val values =
          elemLabels.zip(products).zip(yamlEncoders).map { case ((label, value), yamlWriter) =>
            s"$label: ${yamlWriter.asInstanceOf[YamlWriter[Any]].toYaml(value)}"
          }
        values.mkString("\n")
    }

  inline def sumOf[T](s: Mirror.SumOf[T], yamlEncoders: List[YamlWriter[_]]) =
    new YamlWriter[T]:
      override def toYaml(t: T): String =
        val index = s.ordinal(t)
        yamlEncoders(index).asInstanceOf[YamlWriter[Any]].toYaml(t)

  inline def summonAll[T <: Tuple]: List[YamlWriter[_]] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => summonInline[YamlWriter[t]] :: summonAll[ts]
  }

  inline def getElemLabels[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: EmptyTuple     => Nil
    case _: (head *: tail) => constValue[head].toString :: getElemLabels[tail]
  }
