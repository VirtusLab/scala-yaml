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
}
