package org.virtuslab.yaml

import scala.language.implicitConversions

import org.virtuslab.yaml.Node.*
import org.virtuslab.yaml.*
import org.virtuslab.yaml.syntax.YamlPrimitive

class NodeCreationSuite extends munit.FunSuite:

  test("mapping node") {
    val node = MappingNode(
      "hr"   -> "65",
      "avg"  -> "0.278",
      "blob" -> "nostradamus"
    )

    assertEquals(
      node.asYaml,
      s"""|hr: 65
          |avg: 0.278
          |blob: nostradamus
          |""".stripMargin
    )
  }

  test("sequence node") {
    val node = SequenceNode(
      "65",
      "66",
      "aezakmi"
    )

    assertEquals(
      node.asYaml,
      s"""|- 65
          |- 66
          |- aezakmi
          |""".stripMargin
    )
  }

  test("nested mapping") {
    val node = SequenceNode(
      MappingNode(
        "key" -> "value"
      ),
      MappingNode(
        "key2" -> "value2",
        "seq" -> SequenceNode(
          "v1",
          "v2"
        )
      ),
      "standalone value"
    )

    assertEquals(
      node.asYaml,
      """|- 
         |  key: value
         |- 
         |  key2: value2
         |  seq: 
         |    - v1
         |    - v2
         |- standalone value
         |""".stripMargin
    )
  }

end NodeCreationSuite
