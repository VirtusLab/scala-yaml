package org.virtuslab.yaml
package parser

import org.virtuslab.yaml.internal.load.parse.EventKind._
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.Token
import org.virtuslab.yaml.internal.load.reader.token.TokenKind.MappingKey
import org.virtuslab.yaml.internal.load.parse.NodeEventMetadata

class ParserSuite extends BaseYamlSuite {

  test("kubernetes config") {
    val yaml = s"""apiVersion: v1
                  |kind: Pod
                  |metadata:
                  |  name: iscsipd
                  |spec:
                  |  containers:
                  |  - name: iscsipd-rw
                  |    image: kubernetes/pause
                  |    volumeMounts:
                  |    - mountPath: "/mnt/iscsipd"
                  |      name: iscsipd-rw
                  |  volumes:
                  |  - name: iscsipd-rw
                  |    iscsi:
                  |      targetPortal: 10.0.2.15.3260
                  |      iqn: iqn.2001-04.com.example.storage.kube.sys1.xyz
                  |      lun: 0
                  |      fsType: ext4
                  |      readOnly: true
                  |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("apiVersion"),
      Scalar("v1"),
      Scalar("kind"),
      Scalar("Pod"),
      Scalar("metadata"),
      MappingStart(),
      Scalar("name"),
      Scalar("iscsipd"),
      MappingEnd,
      Scalar("spec"),
      MappingStart(),
      Scalar("containers"),
      SequenceStart(),
      MappingStart(),
      Scalar("name"),
      Scalar("iscsipd-rw"),
      Scalar("image"),
      Scalar("kubernetes/pause"),
      Scalar("volumeMounts"),
      SequenceStart(),
      MappingStart(),
      Scalar("mountPath"),
      Scalar("/mnt/iscsipd", ScalarStyle.DoubleQuoted),
      Scalar("name"),
      Scalar("iscsipd-rw"),
      MappingEnd,
      SequenceEnd,
      MappingEnd,
      SequenceEnd,
      Scalar("volumes"),
      SequenceStart(),
      MappingStart(),
      Scalar("name"),
      Scalar("iscsipd-rw"),
      Scalar("iscsi"),
      MappingStart(),
      Scalar("targetPortal"),
      Scalar("10.0.2.15.3260"),
      Scalar("iqn"),
      Scalar("iqn.2001-04.com.example.storage.kube.sys1.xyz"),
      Scalar("lun"),
      Scalar("0"),
      Scalar("fsType"),
      Scalar("ext4"),
      Scalar("readOnly"),
      Scalar("true"),
      MappingEnd,
      MappingEnd,
      SequenceEnd,
      MappingEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("Parsing error") {
    val errorMessage = """Expected
                         |BlockEnd but instead got MappingKey
                         |  -- zipcode: 12-345
                         |             ^""".stripMargin

    val yaml =
      """name: John Wick
        |age: 40
        |address:
        |  - city: Anywhere
        |  -- zipcode: 12-345
        |""".stripMargin

    val yamlLines = yaml.split("\n", -1).toVector

    assertEquals(
      yaml.asNode,
      Left(
        ParseError.ExpectedTokenKind(
          "BlockEnd",
          Token(
            MappingKey,
            Range(Position(65, 4, 13), yamlLines, None)
          )
        )
      )
    )

    assertEquals(yaml.asNode.left.map(_.msg), Left(errorMessage))
  }

  test("issue 86 - parsing key with empty value") {
    val yaml = """|---
                  |- { "single line", a: b}
                  |- { "multi
                  |  line", a: b}""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(explicit = true),
      SequenceStart(),
      MappingStart(),
      Scalar("single line", ScalarStyle.DoubleQuoted),
      Scalar("", ScalarStyle.Plain, NodeEventMetadata.apply(Tag.nullTag)),
      Scalar("a"),
      Scalar("b"),
      MappingEnd,
      MappingStart(),
      Scalar("multi line", ScalarStyle.DoubleQuoted),
      Scalar("", ScalarStyle.Plain, NodeEventMetadata.apply(Tag.nullTag)),
      Scalar("a"),
      Scalar("b"),
      MappingEnd,
      SequenceEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("issue 313 - parsing elipsis in plain scalar") {
    val yaml = """|P: 
                  |  e: S...
                  |  c: N
                  |""".stripMargin

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("P"),
      MappingStart(),
      Scalar("e"),
      Scalar("S..."),
      Scalar("c"),
      Scalar("N"),
      MappingEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(expectedEvents))
  }

  test("parsing keeps order of keys") {
    val yaml = """
                 |P: 
                 |  a: 0
                 |  b: 1
                 |  c: 2
                 |  d: 3
                 |  e: 4
                 |""".stripMargin

    val node = yaml.asNode.toOption.get

    assertEquals(node.asYaml.trim, yaml.trim)
  }

  test("parseYaml produces yaml node of document") {
    val yaml =
      """name: John Wick
        |age: 40
        |address: 
        |  - Anywhere
        |  - 12-345
        |""".stripMargin

    assertEquals(parseYaml(yaml).toOption.get.asYaml, yaml)
  }

  test("parseManyYamls produces a node for each document") {
    val yaml =
      """one: ah ha ha
        |---
        |two: ah ha ha
        |---
        |three: ah ha ha
        |""".stripMargin

    val nodes = parseManyYamls(yaml).toOption.get

    val actual = nodes.map(_.asYaml).mkString("---\n")

    assertEquals(actual, yaml)
  }
}
