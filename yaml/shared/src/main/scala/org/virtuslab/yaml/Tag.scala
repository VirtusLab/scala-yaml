package org.virtuslab.yaml

import scala.reflect.ClassTag

final case class Tag(value: String)

object Tag:
  def apply[T](implicit classTag: ClassTag[T]): Tag = Tag(s"!${classTag.runtimeClass.getName}")

  private val default = "tag:yaml.org,2002:"
  val nullTag: Tag    = Tag(s"${default}null")
  val boolean: Tag    = Tag(s"${default}bool")
  val int: Tag        = Tag(s"${default}int")
  val float: Tag      = Tag(s"${default}float")
  val str: Tag        = Tag(s"${default}str")
  val seq: Tag        = Tag(s"${default}seq")
  val map: Tag        = Tag(s"${default}map")

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
end Tag
