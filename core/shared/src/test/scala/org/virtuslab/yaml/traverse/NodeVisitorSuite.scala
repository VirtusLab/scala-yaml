package org.virtuslab.yaml.traverse

import org.virtuslab.yaml.TestOps._
import org.virtuslab.yaml._

class NodeVisitorSuite extends munit.FunSuite {

  test("should update ports for web") {

    val yaml =
      s"""version: "3.9"
         |services:
         |  web:
         |    build: .
         |    volumes:
         |      - .:/code
         |      - logvolume01:/var/log
         |    ports:
         |      - "5000:5000"
         |  redis:
         |    image: "redis:alpine"
         |""".stripMargin

    val node: Node = yaml.asNode.orThrow
    val modifiedNode: Node =
      node.modify("services")("web")("ports")(0).setValue("6000:6000").orThrow
    val modifiedYaml = modifiedNode.asYaml

    val exptectedYaml =
      s"""version: 3.9
         |services: 
         |  web: 
         |    build: .
         |    volumes: 
         |      - .:/code
         |      - logvolume01:/var/log
         |    ports: 
         |      - 6000:6000
         |  redis: 
         |    image: redis:alpine
         |""".stripMargin

    assertEquals(modifiedYaml, exptectedYaml)
  }

  test("modify ports for web ") {

    val yaml =
      s"""version: "3.9"
         |services:
         |  web:
         |    build: .
         |    volumes:
         |      - .:/code
         |      - logvolume01:/var/log
         |    ports:
         |      - "5000:5000"
         |  redis:
         |    image: "redis:alpine"
         |""".stripMargin

    val node: Node = yaml.asNode.orThrow
    val modifiedNode: Node =
      node
        .modify("services")("web")("ports")(0)
        .modifyValue((ports) => s"$ports:6000")
        .modify("services")("redis")("image")
        .modifyValue(image => s"$image:latest")
        .orThrow

    val modifiedYaml = modifiedNode.asYaml

    val exptectedYaml =
      s"""version: 3.9
         |services: 
         |  web: 
         |    build: .
         |    volumes: 
         |      - .:/code
         |      - logvolume01:/var/log
         |    ports: 
         |      - 5000:5000:6000
         |  redis: 
         |    image: redis:alpine:latest
         |""".stripMargin

    assertEquals(modifiedYaml, exptectedYaml)
  }

  test("remove ports and image") {

    val yaml =
      s"""version: "3.9"
         |services: 
         |  web: 
         |    build: .
         |    volumes: 
         |      - .:/code
         |      - logvolume01:/var/log
         |    ports:
         |      - "5000:5000"
         |  redis:
         |    image: "redis:alpine"
         |""".stripMargin

    val node: Node = yaml.asNode.orThrow
    val modifiedNode: Node =
      node
        .modify("services")("web")("ports")(0)
        .removeValue()
        .modify("services")("redis")
        .removeValue()
        .orThrow

    val modifiedYaml = modifiedNode.asYaml

    val exptectedYaml =
      s"""version: 3.9
         |services: 
         |  web: 
         |    build: .
         |    volumes: 
         |      - .:/code
         |      - logvolume01:/var/log
         |    ports: 
         |""".stripMargin

    assertEquals(modifiedYaml, exptectedYaml)
  }
}
