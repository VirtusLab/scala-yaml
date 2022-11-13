package org.virtuslab.yaml

final case class Position(
    offset: Int,
    line: Int,
    column: Int
)

final case class Range(
    start: Position,
    input: Vector[String],
    end: Option[Position] = None
) {
  def errorMsg: String = {
    val msg    = input(start.line)
    val spaces = start.column
    s"""|$msg
        |${" " * spaces}^""".stripMargin
  }

  def withEndPos(end: Position): Range =
    copy(
      end = Some(end)
    )

  def show: String = s"Position(${start.line}, ${start.column}), offset ${start.offset}"
}
