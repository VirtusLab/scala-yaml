package org.virtuslab.yaml

case class TestMlEntry(
    description: String,
    from: String,
    tags: String,
    inYaml: String,
    outYaml: Option[String],
    seqEvent: String
)

case object TestMlEntry {

  private def extractInYaml(testMl: String): String = {
    val patternIn =
      raw"--- in-yaml(\(<\)|\(\+\)|\(<\+\)|)(([^\n]*\n+)+?)--- (in-json|error|out-yaml|emit-yaml|test-event)".r

    patternIn
      .findAllIn(testMl)
      .matchData
      .map { m =>
        m.group(2)
      }
      .toList
      .head
  }

  private def extractOutYaml(testMl: String): Option[String] = {
    val patternOut =
      raw"--- out-yaml(\(<\)|\(\+\)|\(<\+\)|)(([^\n]*\n+)+?)--- (in-json|error|emit-yaml|test-event)".r

    patternOut
      .findAllIn(testMl)
      .matchData
      .map { m =>
        m.group(2)
      }
      .toList
      .headOption
      .map(_.strip())
  }

  private def extractSeqEvent(testMl: String): String = {
    val patternEvent = raw"--- test-event(([^\n]*\n+)+).*".r

    patternEvent
      .findFirstIn(testMl)
      .getOrElse(sys.error(s"Error parsing $testMl"))
      .stripPrefix("--- test-event")
      .trim
  }

  def from(testYamlML: os.Path): TestMlEntry = {
    val content = os.read(testYamlML)

    TestMlEntry(
      description = testYamlML.toString,
      from = "",
      tags = "",
      inYaml = extractInYaml(content),
      outYaml = extractOutYaml(content),
      seqEvent = extractSeqEvent(content)
    )
  }
}
