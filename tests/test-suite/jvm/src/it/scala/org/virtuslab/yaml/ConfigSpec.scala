package org.virtuslab.yaml

class ConfigSpec extends YamlRunnerSpec {

  val libYamlPath = os.Path(System.getenv("LIB_YAML_PATH"))

  override def resourcePath: String                   = "/yaml/configs"
  def createTestRunner(yamlPath: os.Path): TestRunner = ConfigTestRunner(yamlPath, libYamlPath)

}
