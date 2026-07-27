package org.virtuslab.yaml

import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle.{DoubleQuoted, SingleQuoted}

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

  private[yaml] val nullPattern   = "^(null|Null|NULL|~)?$".r
  private[yaml] val falsePattern  = "false|False|FALSE".r
  private[yaml] val truePattern   = "true|True|TRUE".r
  private[yaml] val int10Pattern  = "[-+]?[0-9]+".r
  private[yaml] val int8Pattern   = "0o[0-7]+".r
  private[yaml] val int16Pattern  = "0x[0-9a-fA-F]+".r
  private[yaml] val floatPattern  = "[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?".r
  private[yaml] val minusInfinity = "-(\\.inf|\\.Inf|\\.INF)".r
  private[yaml] val plusInfinity  = "\\+?(\\.inf|\\.Inf|\\.INF)".r
  private[yaml] val nan           = "\\.nan|\\.NaN|\\.NAN".r

  def resolveTag(value: String, style: Option[ScalarStyle] = None): Tag =
    if (value eq null) nullTag
    else if (style.exists(s => s == DoubleQuoted || s == SingleQuoted)) str
    else if (value.isEmpty) nullTag
    else {
      value.charAt(0) match {
        case 'n' | 'N' | '~' =>
          if (nullPattern.matches(value)) nullTag
          else str
        case 'f' | 'F' =>
          if (falsePattern.matches(value)) boolean
          else str
        case 't' | 'T' =>
          if (truePattern.matches(value)) boolean
          else str
        case '-' =>
          if (int10Pattern.matches(value)) int
          else if (floatPattern.matches(value)) float
          else if (minusInfinity.matches(value)) float
          else str
        case '+' =>
          if (int10Pattern.matches(value)) int
          else if (floatPattern.matches(value)) float
          else if (plusInfinity.matches(value)) float
          else str
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          if (int10Pattern.matches(value)) int
          else if (int8Pattern.matches(value)) int
          else if (int16Pattern.matches(value)) int
          else if (floatPattern.matches(value)) float
          else str
        case '.' =>
          if (floatPattern.matches(value)) float
          else if (plusInfinity.matches(value)) float
          else if (nan.matches(value)) float
          else str
        case _ => str
      }
    }
}
