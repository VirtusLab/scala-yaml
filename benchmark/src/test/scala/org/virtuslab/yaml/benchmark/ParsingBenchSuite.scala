package org.virtuslab.yaml.benchmark

class ParsingBenchSuite extends munit.FunSuite {
  test("docker-compose.yaml from string to events") {
    val bench = new ParsingBench
    bench.name = "docker-compose.yaml"
    bench.setup()
    assertEquals(bench.fromStringToEvents.isRight, true)
  }

  test("docker-compose.yaml from string to node") {
    val bench = new ParsingBench
    bench.name = "docker-compose.yaml"
    bench.setup()
    assertEquals(bench.fromStringToEvents.isRight, true)
  }

  test("docker-compose.yaml from string to struct") {
    val bench = new ParsingBench
    bench.name = "docker-compose.yaml"
    bench.setup()
    assertEquals(bench.fromStringToStruct.isRight, true)
  }

  test("geo.yaml from string to events") {
    val bench = new ParsingBench
    bench.name = "geo.yaml"
    bench.setup()
    assertEquals(bench.fromStringToEvents.isRight, true)
  }

  test("geo.yaml from string to node") {
    val bench = new ParsingBench
    bench.name = "geo.yaml"
    bench.setup()
    assertEquals(bench.fromStringToEvents.isRight, true)
  }

  test("geo.yaml from string to struct") {
    val bench = new ParsingBench
    bench.name = "geo.yaml"
    bench.setup()
    assertEquals(bench.fromStringToStruct.isRight, true)
  }
}
