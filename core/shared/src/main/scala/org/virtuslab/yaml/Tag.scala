package org.virtuslab.yaml

import scala.reflect.ClassTag

sealed trait Tag {
  def value: String
}

final case class CoreSchemaTag(value: String) extends Tag
final case class CustomTag(value: String)     extends Tag

object Tag {
  def apply[T](implicit classTag: ClassTag[T]): Tag = CustomTag(
    s"!${classTag.runtimeClass.getName}"
  )

  private val default = "tag:yaml.org,2002:"
  val nullTag: Tag    = CoreSchemaTag(s"${default}null")
  val boolean: Tag    = CoreSchemaTag(s"${default}bool")
  val int: Tag        = CoreSchemaTag(s"${default}int")
  val float: Tag      = CoreSchemaTag(s"${default}float")
  val str: Tag        = CoreSchemaTag(s"${default}str")
  val seq: Tag        = CoreSchemaTag(s"${default}seq")
  val map: Tag        = CoreSchemaTag(s"${default}map")

  val corePrimitives   = Set(nullTag, boolean, int, float, str)
  val coreSchemaValues = (corePrimitives ++ Set(seq, map)).map(_.value)

  private val nullPattern    = "null|Null|NULL|~".r
  private val booleanPattern = "true|True|TRUE|false|False|FALSE".r
  private val int10Pattern   = "[-+]?[0-9]+".r
  private val int8Pattern    = "0o[0-7]+".r
  private val int16Pattern   = "0x[0-9a-fA-F]+".r
  private val floatPattern   = "[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?".r
  private val minusInfinity  = "-(\\.inf|\\.Inf|\\.INF)".r
  private val plusInfinity   = "\\+?(\\.inf|\\.Inf|\\.INF)".r

  def resolveTag(value: String): Tag = {
    value match {
      case null               => nullTag
      case nullPattern(_*)    => nullTag
      case booleanPattern(_*) => boolean
      case int10Pattern(_*)   => int
      case int8Pattern(_*)    => int
      case int16Pattern(_*)   => int
      case floatPattern(_*)   => float
      case minusInfinity(_*)  => float
      case plusInfinity(_*)   => float
      case _                  => str
    }
  }
}
