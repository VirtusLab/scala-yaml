package org.virtuslab.yaml.internal.load

enum TagHandle:
  case Primary
  case Secondary
  case Named(name: String)

enum TagPrefix:
  case Local(tag: String)
  case Global(tag: String)


enum TagValue:
  case NonSpecific
  case Verbatim(value: String)
  case Shorthand(handle: TagHandle, rest: String)

opaque type TagSuffix = String
object TagSuffix:
  def apply(suffix: String): TagSuffix = suffix
