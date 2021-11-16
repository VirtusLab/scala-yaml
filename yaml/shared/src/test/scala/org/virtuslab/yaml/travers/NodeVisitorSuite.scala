package org.virtuslab.yaml.travers

import org.virtuslab.yaml.*
import org.virtuslab.yaml.TestOps.*
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.syntax.NodeVisitor._

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
      node
        .modify("services")("web")("ports")(0)
        .setValue("6000:6000")
        .modify("services")("redis")("image")
        .setValue("openjdk:11")
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
         |      - 6000:6000
         |  redis: 
         |    image: openjdk:11
         |""".stripMargin

    assertEquals(modifiedYaml, exptectedYaml)
  }
}
