package org.virtuslab.yaml.internal.load

enum TagHandle:
  case Primary
  case Secondary
  case Named(name: String)

enum TagPrefix(val tag: String):
  case Local(override val tag: String) extends TagPrefix(tag)
  case Global(override val tag: String) extends TagPrefix(tag)

enum TagValue:
  case NonSpecific
  case Verbatim(value: String)
  case Shorthand(handle: TagHandle, rest: String)

opaque type TagSuffix = String
object TagSuffix:
  def apply(suffix: String): TagSuffix = suffix
