package org.virtuslab.yaml

case class TestMlEntry(
    description: String,
    from: String,
    tags: String,
    inYaml: String,
    testEvent: String
)

case object TestMlEntry {
  def from(testMl: String): TestMlEntry = {

    val patternIn =
      raw"--- in-yaml(\(<\)|)(([^\n]*\n+)+?)--- (in-json|error|out-yaml|emit-yaml|test-event)".r
    val patternEvent = raw"--- test-event(([^\n]*\n+)+).*".r

    val inYaml: String =
      patternIn
        .findAllIn(testMl)
        .matchData
        .map { m =>
          m.group(2)
        }
        .toList
        .head

    val eventYaml: String =
      patternEvent
        .findFirstIn(testMl)
        .getOrElse(sys.error(s"Error parsing $testMl"))
        .stripPrefix("--- test-event")
        .trim

    TestMlEntry(
      description = "",
      from = "",
      tags = "",
      inYaml = inYaml,
      testEvent = eventYaml
    )
  }
}
