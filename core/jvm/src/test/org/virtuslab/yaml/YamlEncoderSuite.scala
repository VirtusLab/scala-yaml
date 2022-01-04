package org.virtuslab.yaml

import org.virtuslab.yaml.*

class YamlEncoderSuite extends munit.FunSuite:

  test("case class (mapping)") {
    case class Stats(hr: Int, avg: Double, rbi: Int) derives YamlCodec
    val data = Stats(1, 1.0, 1)
    val expected =
      s"""hr: 1
         |avg: 1.0
         |rbi: 1
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
