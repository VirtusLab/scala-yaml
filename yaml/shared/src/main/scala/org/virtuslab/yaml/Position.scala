package org.virtuslab.yaml

final case class Position(offset: Int, line: Int, column: Int, input: Vector[String]):
  def errorMsg: String =
    val msg          = input(line - 1)
    val spaces       = column - 1
    val circumflexes = msg.length - spaces
    s"""|$msg
        |${" " * spaces}${"^" * circumflexes}""".stripMargin
