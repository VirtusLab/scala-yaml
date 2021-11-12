package org.virtuslab.yaml.internal.load

enum TagHandle(val value: String):
  case Primary extends TagHandle("!")
  case Secondary extends TagHandle("!!")
  case Named(name: String) extends TagHandle(name)

enum TagPrefix(val value: String):
  case Local(prefix: String) extends TagPrefix(prefix)
  case Global(prefix: String) extends TagPrefix(prefix)

enum TagValue:
  case NonSpecific
  case Verbatim(value: String)
  case Shorthand(handle: TagHandle, rest: String)

opaque type TagSuffix = String
object TagSuffix:
  def apply(suffix: String): TagSuffix = suffix
