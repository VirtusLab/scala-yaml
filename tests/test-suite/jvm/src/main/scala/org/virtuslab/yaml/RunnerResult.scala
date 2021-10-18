package org.virtuslab.yaml

trait RunnerResult:
  def isValid: Boolean

case object RunnerResult:
  
  case class Success(events: String, expectedEvents: String) extends RunnerResult:
    override def isValid: Boolean = events == expectedEvents

  case class Exception(events: String, yamlError: YamlError) extends RunnerResult:
    override val isValid: Boolean = false

end RunnerResult