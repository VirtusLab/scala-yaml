package org.virtuslab.yaml.internal

import org.virtuslab.yaml.*
import org.virtuslab.yaml.Node.*

class NodeCreationSuite extends munit.FunSuite:

  test("mapping-node") {
    val node = MappingNode(
      "hr"   -> ScalarNode("65"),
      "avg"  -> ScalarNode("0.278"),
      "blob" -> ScalarNode("nostradamus")
    )

    assertEquals(
      node.asYaml,
      s"""|hr: 65
          |avg: 0.278
          |blob: nostradamus
          |""".stripMargin
    )
  }

  test("sequence-node") {
    val node = SequenceNode(
      ScalarNode("65"),
      ScalarNode("66"),
      ScalarNode("aezakmi")
    )

    assertEquals(
      node.asYaml,
      s"""|- 65
          |- 66
          |- aezakmi
          |""".stripMargin
    )
  }

  test("scalar-node") {
    val node = ScalarNode("65")

    assertEquals(
      node.asYaml,
      """|65
         |""".stripMargin
    )
  }

  test("nested") {
    val node = SequenceNode(
      MappingNode(
        "key" -> ScalarNode("value")
      ),
      MappingNode(
        "key2" -> ScalarNode("value2"),
        "seq" -> SequenceNode(
          ScalarNode("v1"),
          ScalarNode("v2")
        )
      ),
      ScalarNode("standalone value")
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
