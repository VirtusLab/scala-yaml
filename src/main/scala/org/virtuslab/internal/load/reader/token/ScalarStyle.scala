package org.virtuslab.internal.load.reader.token

enum ScalarStyle(character: Char):
  case Plain extends ScalarStyle(' ')
  case DoubleQuoted extends ScalarStyle('"')
  case SingleQuoted extends ScalarStyle('\'')
  case Folded extends ScalarStyle('>')
end ScalarStyle
