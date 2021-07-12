package org.virtuslab.internal.load.reader.token

enum ScalarStyle(character: Char):
  case Plain extends ScalarStyle(' ')
  case DopubleQuoted extends ScalarStyle('"')
  case SingleQuoted extends ScalarStyle('\'')
end ScalarStyle
