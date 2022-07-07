package org.virtuslab.yaml

import org.virtuslab.yaml.internal.load.parse.EventKind

trait RunnerResult {
  def isValid: Boolean
}

object RunnerResult {

  def apply(events: List[EventKind], expectedEvents: String): RunnerResult = {
    val eventsAsStr = TestRunnerUtils.convertEventToYamlTestSuiteFormat(events)
    if (eventsAsStr == expectedEvents) Success(eventsAsStr)
    else InvalidEvents(eventsAsStr, expectedEvents)
  }

  def apply(events: List[EventKind], expectedEvents: String, error: YamlError): RunnerResult = {
    val eventsAsStr = TestRunnerUtils.convertEventToYamlTestSuiteFormat(events)
    if (eventsAsStr == expectedEvents) Success(eventsAsStr)
    else Error(eventsAsStr, error)
  }

  case class Success(events: String) extends RunnerResult {
    override val isValid = true
  }

  case class InvalidEvents(events: String, expectedEvents: String) extends RunnerResult {
    override val isValid = false
  }

  case class Error(events: String, yamlError: YamlError) extends RunnerResult {
    override val isValid = false
  }

}
