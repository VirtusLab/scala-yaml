package org.virtuslab.yaml.internal.load.reader.token

enum FinalBreak(indicator: Char):
  /**
   * Final break is excluded from content
   */
  case Keep extends FinalBreak('+')

  /**
   * Final break and any trailing empty lines are included to content
   */
  case Strip extends FinalBreak('-')

  /**
    * Final break is included to content, rest of empty lines are excluded. Default behaviour
   */
  case Clip extends FinalBreak(' ')
end FinalBreak

case object FinalBreak {

  def choompedClipType(content: String): String =
    content.takeRight(2) match
      case "\n\n" =>
        choompedClipType(content.dropRight(1))
      case _ => content

  def choompedStripType(content: String): String =
    content.takeRight(1) match
      case "\n" =>
        choompedClipType(content.dropRight(1))
      case _ => content

  def choompedKeepType(content: String): String = content

}
