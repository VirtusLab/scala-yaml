package org.virtuslab.internal.load.reader.token

enum ScalarStyle(indicator: Char):
  case Plain extends ScalarStyle(' ')
  case DoubleQuoted extends ScalarStyle('"')
  case SingleQuoted extends ScalarStyle('\'')
  case Folded extends ScalarStyle('>')
  case Literal extends ScalarStyle('|')
end ScalarStyle
