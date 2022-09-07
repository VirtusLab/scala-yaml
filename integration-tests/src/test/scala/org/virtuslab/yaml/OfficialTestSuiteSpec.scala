package org.virtuslab.yaml

class OfficialTestSuiteSpec extends YamlRunnerSpec {

  override def resourcePath: String                            = "/yaml/test-suite"
  override def createTestRunner(yamlPath: os.Path): TestRunner = YamlSuiteTestRunner(yamlPath)
}
