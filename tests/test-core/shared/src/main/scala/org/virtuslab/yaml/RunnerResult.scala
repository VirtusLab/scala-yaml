package org.virtuslab.yaml

trait RunnerResult:
  def isValid: Boolean

case object RunnerResult:

  case class Success(events: String, expectedEvents: String) extends RunnerResult {
    override def isValid: Boolean = events == expectedEvents
  }
  case class Exeception(yamlError: YamlError) extends RunnerResult {
    override def isValid: Boolean = false
  }
