package org.virtuslab.yaml.internal.load.reader.token

/** Chomping controls how final line breaks and trailing empty lines are interpreted. YAML provides
  * three chomping methods:
  *
  * 1. Strip - stripping is specified by the “-” chomping indicator. In this case, the final line
  * break and any trailing empty lines are excluded from the scalar’s content.
  *
  * 2. Clip - clipping is the default behavior used if no explicit chomping indicator is specified.
  * In this case, the final line break character is preserved in the scalar’s content. However, any
  * trailing empty lines are excluded from the scalar’s content.
  *
  * 3. Keep - keeping is specified by the “+” chomping indicator. In this case, the final line break
  * and any trailing empty lines are considered to be part of the scalar’s content. These additional
  * lines are not subject to folding.
  */
sealed abstract class BlockChompingIndicator(indicator: Char) {
  def removeBlankLinesAtEnd(scalar: String): String
}

case object BlockChompingIndicator {

  /**
   * Final break and any trailing empty lines are included to content
   * key: scalar
         (this and the following empty lines will be added to content)
   */
  case object Keep extends BlockChompingIndicator('+') {
    override def removeBlankLinesAtEnd(scalar: String): String = scalar
  }

  /**
   * Final break is excluded from content
   * key: scalar
       (this and the following empty lines will be dropped)
   */
  case object Strip extends BlockChompingIndicator('-') {
    override def removeBlankLinesAtEnd(scalar: String): String =
      scalar.takeRight(1) match {
        case "\n" =>
          removeBlankLinesAtEnd(scalar.dropRight(1))
        case _ => scalar
      }
  }

  /**
    * Final break is included to content, rest of empty lines are excluded. Default behaviour
   * key: key
       (preserve empty line)
       (this and the following empty lines will be dropped)
   */
  case object Clip extends BlockChompingIndicator(' ') {
    override def removeBlankLinesAtEnd(scalar: String): String =
      scalar.takeRight(2) match {
        case "\n\n" =>
          removeBlankLinesAtEnd(scalar.dropRight(1))
        case _ => scalar
      }
  }

}
