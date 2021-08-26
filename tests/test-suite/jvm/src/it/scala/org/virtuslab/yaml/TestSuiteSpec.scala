package org.virtuslab.yaml

class TestSuiteSpec extends YamlRunnerSpec:

  override def resourcePath: String                            = "/yaml/test-suite"
  override def createTestRunner(yamlPath: os.Path): TestRunner = TestSuiteRunner(yamlPath)
