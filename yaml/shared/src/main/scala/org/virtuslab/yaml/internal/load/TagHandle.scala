package org.virtuslab.yaml.internal.load

enum TagHandle:
  case Primary
  case Secondary
  case Named(name: String)

  def value: String = this match
    case Primary     => "!"
    case Secondary   => "!!"
    case Named(name) => name
end TagHandle

enum TagPrefix:
  case Local(prefix: String)
  case Global(prefix: String)

  def value: String = this match
    case Local(prefix)  => prefix
    case Global(prefix) => prefix
end TagPrefix

enum TagValue:
  case NonSpecific
  case Verbatim(value: String)
  case Shorthand(handle: TagHandle, rest: String)

opaque type TagSuffix = String
object TagSuffix:
  def apply(suffix: String): TagSuffix = suffix
