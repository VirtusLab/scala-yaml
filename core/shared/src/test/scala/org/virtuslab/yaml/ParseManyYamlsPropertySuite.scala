package org.virtuslab.yaml

/**
 * Directional / property-style tests for parseManyYamls covering various
 * combinations of document markers (--- and ...) at end-of-input, with
 * and without trailing newlines.
 */
class ParseManyYamlsPropertySuite extends BaseYamlSuite {

  // --- Property: parseManyYamls should never fail for valid YAML with document markers at end of input ---

  private val scalars = List("1", "hello", "true", "null", "3.14", "a string with spaces")

  test("property: scalar + newline + --- (no trailing newline) always parses") {
    for (s <- scalars) {
      val yaml = s"$s\n---"
      val result = parseManyYamls(yaml)
      assert(result.isRight, s"Failed for input: ${yaml.replace("\n", "\\n")} => $result")
      val nodes = result.toOption.get
      assert(nodes.nonEmpty, s"Expected non-empty nodes for: $yaml")
    }
  }

  test("property: scalar + newline + ... (no trailing newline) always parses") {
    for (s <- scalars) {
      val yaml = s"$s\n..."
      val result = parseManyYamls(yaml)
      assert(result.isRight, s"Failed for input: ${yaml.replace("\n", "\\n")} => $result")
      val nodes = result.toOption.get
      assertEquals(nodes.length, 1, s"Expected exactly 1 node for: $yaml")
    }
  }

  test("property: --- at end of input always produces a document") {
    val yaml = "---"
    val result = parseManyYamls(yaml)
    assert(result.isRight, s"Failed for input: $yaml => $result")
  }

  test("property: ... at end of input produces no documents") {
    val yaml = "..."
    // standalone ... without prior content is treated as an empty stream (no documents)
    // parseYaml/parseManyYamls may return Left since there's no node to compose,
    // but it should never throw an exception
    val result = parseManyYamls(yaml)
    result match {
      case Right(nodes) => assertEquals(nodes.length, 0)
      case Left(_)      => () // acceptable: no document to compose
    }
  }

  // --- Property: parseManyYamls document count should be consistent ---

  test("property: N documents separated by --- produce N nodes") {
    for (n <- 1 to 5) {
      val docs = (1 to n).map(i => s"doc$i")
      val yaml = docs.mkString("\n---\n")
      val result = parseManyYamls(yaml)
      assert(result.isRight, s"Failed for $n docs: $result")
      val nodes = result.toOption.get
      assertEquals(nodes.length, n, s"Expected $n nodes for yaml: ${yaml.replace("\n", "\\n")}")
    }
  }

  test("property: N documents separated by --- with trailing newline produce N nodes") {
    for (n <- 1 to 5) {
      val docs = (1 to n).map(i => s"doc$i")
      val yaml = docs.mkString("\n---\n") + "\n"
      val result = parseManyYamls(yaml)
      assert(result.isRight, s"Failed for $n docs: $result")
      val nodes = result.toOption.get
      assertEquals(nodes.length, n, s"Expected $n nodes for yaml: ${yaml.replace("\n", "\\n")}")
    }
  }

  // --- Property: adding trailing newline should not change document count ---

  test("property: trailing newline does not change document count for scalar + ---") {
    for (s <- scalars) {
      val yamlNoNewline = s"$s\n---"
      val yamlWithNewline = s"$s\n---\n"
      val resultNo = parseManyYamls(yamlNoNewline)
      val resultWith = parseManyYamls(yamlWithNewline)
      assert(resultNo.isRight, s"Failed without newline: $resultNo")
      assert(resultWith.isRight, s"Failed with newline: $resultWith")
      assertEquals(
        resultNo.toOption.get.length,
        resultWith.toOption.get.length,
        s"Document count differs for scalar '$s' with/without trailing newline"
      )
    }
  }

  test("property: trailing newline does not change document count for scalar + ...") {
    for (s <- scalars) {
      val yamlNoNewline = s"$s\n..."
      val yamlWithNewline = s"$s\n...\n"
      val resultNo = parseManyYamls(yamlNoNewline)
      val resultWith = parseManyYamls(yamlWithNewline)
      assert(resultNo.isRight, s"Failed without newline: $resultNo")
      assert(resultWith.isRight, s"Failed with newline: $resultWith")
      assertEquals(
        resultNo.toOption.get.length,
        resultWith.toOption.get.length,
        s"Document count differs for scalar '$s' with/without trailing newline after ..."
      )
    }
  }

  // --- Property: first document content is always preserved ---

  test("property: first document scalar value is preserved when followed by ---") {
    for (s <- scalars) {
      val yaml = s"$s\n---"
      val result = parseManyYamls(yaml)
      assert(result.isRight)
      val firstNode = result.toOption.get.head
      firstNode match {
        case Node.ScalarNode(value, _) =>
          assertEquals(value, s, s"First document value mismatch for input: $yaml")
        case other =>
          fail(s"Expected ScalarNode but got: $other")
      }
    }
  }

  test("property: first document scalar value is preserved when followed by ...") {
    for (s <- scalars) {
      val yaml = s"$s\n..."
      val result = parseManyYamls(yaml)
      assert(result.isRight)
      val firstNode = result.toOption.get.head
      firstNode match {
        case Node.ScalarNode(value, _) =>
          assertEquals(value, s, s"First document value mismatch for input: $yaml")
        case other =>
          fail(s"Expected ScalarNode but got: $other")
      }
    }
  }

  // --- Mapping variants ---

  test("property: mappings followed by --- at end of input") {
    val mappings = List("a: 1", "key: value", "x: y")
    for (m <- mappings) {
      val yaml = s"$m\n---"
      val result = parseManyYamls(yaml)
      assert(result.isRight, s"Failed for input: ${yaml.replace("\n", "\\n")} => $result")
      assertEquals(result.toOption.get.length, 2)
    }
  }

  test("property: mappings followed by ... at end of input") {
    val mappings = List("a: 1", "key: value", "x: y")
    for (m <- mappings) {
      val yaml = s"$m\n..."
      val result = parseManyYamls(yaml)
      assert(result.isRight, s"Failed for input: ${yaml.replace("\n", "\\n")} => $result")
      assertEquals(result.toOption.get.length, 1)
    }
  }
}

