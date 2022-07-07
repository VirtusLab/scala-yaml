package org.virtuslab.yaml.internal.load

sealed abstract class TagHandle(val value: String)
object TagHandle {
  case object Primary            extends TagHandle("!")
  case object Secondary          extends TagHandle("!!")
  case class Named(name: String) extends TagHandle(name)
}

sealed abstract class TagPrefix(val value: String)
object TagPrefix {
  case class Local(prefix: String)  extends TagPrefix(prefix)
  case class Global(prefix: String) extends TagPrefix(prefix)
}

sealed abstract class TagValue
object TagValue {
  case object NonSpecific                               extends TagValue
  case class Verbatim(value: String)                    extends TagValue
  case class Shorthand(handle: TagHandle, rest: String) extends TagValue
}

final class TagSuffix(val suffix: String) extends AnyVal
object TagSuffix {
  def apply(suffix: String): TagSuffix = new TagSuffix(suffix)
}
