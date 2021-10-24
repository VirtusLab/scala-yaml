package org.virtuslab.yaml.internal.load.reader.token

enum ScalarStyle(indicator: Char):
  case Plain extends ScalarStyle(' ')
  case DoubleQuoted extends ScalarStyle('"')
  case SingleQuoted extends ScalarStyle('\'')
  case Folded extends ScalarStyle('>')
  case Literal extends ScalarStyle('|')
end ScalarStyle

case object ScalarStyle:
  def escapeSpecialCharacter(scalar: String, scalarStyle: ScalarStyle): String =
    scalarStyle match
      case ScalarStyle.DoubleQuoted => scalar
      case _ =>
        scalar.flatMap { char =>
          char match
            case '\\'  => "\\\\"
            case '\n'  => "\\n"
            case other => other.toString
        }

  def escapeSpecialCharacterDoubleQuote(scalar: String) =
    scalar.flatMap { char =>
      char match
        case '\n'  => ""
        case other => other.toString
    }
