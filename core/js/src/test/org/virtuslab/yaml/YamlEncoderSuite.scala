package org.virtuslab.yaml

import org.virtuslab.yaml.*

class YamlEncoderSpec extends munit.FunSuite:

  test("sequence of sequences") {
    val data = Seq(
      Seq(1, 2),
      Seq(3, 4)
    )

    val expected =
      s"""- 
         |  - 1
         |  - 2
         |- 
         |  - 3
         |  - 4
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }

  test("mapping of sequences") {
    case class Data(ints: Seq[Int], doubles: Seq[Double]) derives YamlCodec
    val data = Data(Seq(1, 2), Seq(3.0, 4.0))

    val expected =
      s"""ints: 
         |  - 1
         |  - 2
         |doubles: 
         |  - 3.0
         |  - 4.0
         |""".stripMargin

    assertEquals(data.asYaml, expected)
  }
