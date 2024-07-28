### Test suite

Yaml test suite are used in integration testing - https://github.com/yaml/yaml-test-suite

#### How to run integration tests

1. Figure out if `yamlpp-events` is installed on your system or install it if not, it's perl's yaml pp module
2. Run `downloadYamlConfigs.sh` script to download yaml test suites
3. `sbt integration/test` to run tests