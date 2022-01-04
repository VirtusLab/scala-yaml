package org.virtuslab.yaml.internal.load.reader.token

sealed trait BlockChompingIndicator(indicator: Char):
  def removeBlankLinesAtEnd(scalar: String): String

case object BlockChompingIndicator {

  /**
   * Final break and any trailing empty lines are included to content
   * key: scalar
         (this and the following empty lines will be added to content)
   */
  case object Keep extends BlockChompingIndicator('+'):
    override def removeBlankLinesAtEnd(scalar: String): String = scalar

  /**
   * Final break is excluded from content
   * key: scalar
       (this and the following empty lines will be dropped)
   */
  case object Strip extends BlockChompingIndicator('-'):
    override def removeBlankLinesAtEnd(scalar: String): String =
      scalar.takeRight(1) match
        case "\n" =>
          removeBlankLinesAtEnd(scalar.dropRight(1))
        case _ => scalar

  /**
    * Final break is included to content, rest of empty lines are excluded. Default behaviour
   * key: key
       (preserve empty line)
       (this and the following empty lines will be dropped)
   */
  case object Clip extends BlockChompingIndicator(' '):
    override def removeBlankLinesAtEnd(scalar: String): String =
      scalar.takeRight(2) match
        case "\n\n" =>
          removeBlankLinesAtEnd(scalar.dropRight(1))
        case _ => scalar

}
