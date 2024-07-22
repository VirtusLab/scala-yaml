package org.virtuslab.yaml

import scala.util.matching.Regex

class OfficialTestSuiteSpec extends YamlRunnerSpec {

  override val verbose: Boolean           = true
  override val testOutYaml: Boolean       = false
  override val skipErrors: Boolean        = false
  override val skipInvalidEvents: Boolean = false

  // override def runMatching: Regex = ".*LQZ7.*".r

  override def resourcePath: String = "/yaml/test-suite"
  override def createTestRunner(yamlPath: os.Path): TestRunner =
    YamlSuiteTestRunner(yamlPath, testOutYaml)
}
