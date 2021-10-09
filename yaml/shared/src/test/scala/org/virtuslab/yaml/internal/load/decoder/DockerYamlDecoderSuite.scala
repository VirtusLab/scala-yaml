package org.virtuslab.yaml.internal.load.decoder

import org.virtuslab.yaml.*

class DockerYamlDecoderSuite extends munit.FunSuite:

  test("derive construct for docker compose file".ignore) {

    case class Web(build: String, ports: List[String], volumes: List[String]) derives YamlCodec
    case class Redis(image: String) derives YamlCodec
    case class Services(web: Web, redis: Redis) derives YamlCodec
    case class Compose(version: String, services: Services) derives YamlCodec

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

    val expectedCompose = Compose(
      version = "3.9",
      services = Services(
        web = Web(
          build = ".",
          ports = List("5000:5000"),
          volumes = List(".:/code", "logvolume01:/var/log")
        ),
        redis = Redis(
          image = "redis:alpine"
        )
      )
    )

    assertEquals(yaml.as[Compose], Right(expectedCompose))
  }
