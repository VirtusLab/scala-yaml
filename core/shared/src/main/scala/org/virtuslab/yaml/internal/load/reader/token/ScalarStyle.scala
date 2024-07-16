package org.virtuslab.yaml.internal.load.reader.token

sealed abstract class ScalarStyle(indicator: Char)
object ScalarStyle {
  case object Plain        extends ScalarStyle(' ')
  case object DoubleQuoted extends ScalarStyle('"')
  case object SingleQuoted extends ScalarStyle('\'')
  case object Folded       extends ScalarStyle('>')
  case object Literal      extends ScalarStyle('|')

  def escapeSpecialCharacter(scalar: String, scalarStyle: ScalarStyle): String =
    scalarStyle match {
      case ScalarStyle.DoubleQuoted => scalar
      case ScalarStyle.SingleQuoted => scalar
      case ScalarStyle.Literal      => scalar
      case _ =>
        scalar.flatMap { char =>
          char match {
            case '\\'  => "\\\\"
            case '\n'  => "\\n"
            case other => other.toString
          }
        }
    }

  def escapeSpecialCharacterDoubleQuote(scalar: String) =
    scalar.flatMap { char =>
      char match {
        case '\n'  => ""
        case other => other.toString
      }
    }
}
