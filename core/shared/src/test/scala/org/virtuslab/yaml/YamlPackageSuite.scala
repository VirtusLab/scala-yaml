package org.virtuslab.yaml

class YamlPackageSuite extends BaseYamlSuite {
  test("asMany fails on parse failure") {
    val yaml   = "["
    val actual = yaml.asMany[Int]
    assert(actual.isLeft)
  }

  test("asMany fails on first decoder failure") {
    val yaml = """
               |123
               |---
               |not an Int
               |""".stripMargin

    val actual = yaml.asMany[Int]
    assert(actual.isLeft)
    val errorMessage = actual.left.get.getMessage()
    assert(errorMessage.contains("not an Int"))
  }

  test("asMany succeeds with multiple valid documents") {
    val yaml = """
               |123
               |---
               |42
               |---
               |256
               |""".stripMargin

    val actual = yaml.asMany[Int]
    assert(actual.isRight)
    val ints = actual.right.get
    assertEquals(ints, List(123, 42, 256))
  }

  test("asMany handles scalar followed by --- at end of input") {
    val yaml = "123\n---"
    val nodes = parseManyYamls(yaml)
    assert(nodes.isRight, s"parseManyYamls failed: $nodes")
    assertEquals(nodes.toOption.get.length, 2)
    val firstDoc = nodes.toOption.get.head.as[Int]
    assertEquals(firstDoc, Right(123))
  }

  test("asMany handles scalar followed by ... at end of input") {
    val yaml = "hello\n..."
    val actual = yaml.asMany[String]
    assert(actual.isRight, s"asMany failed: $actual")
    assertEquals(actual.toOption.get, List("hello"))
  }

  test("asMany handles multiple documents without trailing newline") {
    val yaml = "a\n---\nb\n---\nc"
    val actual = yaml.asMany[String]
    assert(actual.isRight, s"asMany failed: $actual")
    assertEquals(actual.toOption.get, List("a", "b", "c"))
  }
}
