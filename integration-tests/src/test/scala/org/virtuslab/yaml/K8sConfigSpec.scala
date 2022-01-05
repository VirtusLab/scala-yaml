package org.virtuslab.yaml

class K8sConfigSpec extends YamlRunnerSpec {

  val libYamlPath = os.Path(System.getenv("LIB_YAML_PATH"))

  override def resourcePath: String                   = "/yaml/configs"
  def createTestRunner(yamlPath: os.Path): TestRunner = K8sYamlTestRunner(yamlPath, libYamlPath)

}
