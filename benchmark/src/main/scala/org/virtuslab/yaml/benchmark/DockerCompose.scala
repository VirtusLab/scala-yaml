package org.virtuslab.yaml.benchmark

import org.virtuslab.yaml.*

object DockerCompose {
  enum Environment {
    case AsMap(map: Map[String, String])
    case AsList(list: List[String])
  }

  enum Command {
    case AsString(cmd: String)
    case AsList(cmd: List[String])
  }

  enum DependsOn {
    case AsList(list: List[String])
    case AsMap(map: Map[String, DependsOnCondition])
  }

  given YamlDecoder[Environment] = new YamlDecoder[Environment] {
    override def construct(
        node: Node
    )(implicit settings: LoadSettings = LoadSettings.empty): Either[ConstructError, Environment] = {
      node.as[Map[String, String]] match {
        case Right(map) => Right(Environment.AsMap(map))
        case Left(_) =>
          node.as[List[String]] match {
            case Right(list) => Right(Environment.AsList(list))
            case Left(err) =>
              Left(ConstructError(s"Expected Map or List for environment: $err", Some(node), None))
          }
      }
    }
  }

  given YamlDecoder[Command] = new YamlDecoder[Command] {
    override def construct(
        node: Node
    )(implicit settings: LoadSettings = LoadSettings.empty): Either[ConstructError, Command] = {
      node.as[String] match {
        case Right(str) => Right(Command.AsString(str))
        case Left(_) =>
          node.as[List[String]] match {
            case Right(list) => Right(Command.AsList(list))
            case Left(err) =>
              Left(ConstructError(s"Expected String or List for command: $err", Some(node), None))
          }
      }
    }
  }

  given YamlDecoder[DependsOn] = new YamlDecoder[DependsOn] {
    override def construct(
        node: Node
    )(implicit settings: LoadSettings = LoadSettings.empty): Either[ConstructError, DependsOn] = {
      node.as[Map[String, DependsOnCondition]] match {
        case Right(map) => Right(DependsOn.AsMap(map))
        case Left(_) =>
          node.as[List[String]] match {
            case Right(list) => Right(DependsOn.AsList(list))
            case Left(err) =>
              Left(ConstructError(s"Expected Map or List for depends_on: $err", Some(node), None))
          }
      }
    }
  }

  given YamlEncoder[Command] = new YamlEncoder[Command] {
    override def asNode(obj: Command): Node = obj match {
      case Command.AsString(cmd)   => summon[YamlEncoder[String]].asNode(cmd)
      case Command.AsList(cmdList) => summon[YamlEncoder[List[String]]].asNode(cmdList)
    }
  }

  case class DependsOnCondition(condition: Option[String]) derives YamlDecoder

  case class Logging(driver: Option[String], options: Option[Map[String, String]])
      derives YamlDecoder

  case class Healthcheck(
      test: Option[List[String]],
      interval: Option[String],
      timeout: Option[String],
      retries: Option[Int]
  ) derives YamlDecoder

  case class ResourceLimits(cpus: Option[String], memory: Option[String]) derives YamlDecoder

  case class Resources(limits: Option[ResourceLimits]) derives YamlDecoder

  case class Deploy(replicas: Option[Int], resources: Option[Resources]) derives YamlDecoder

  case class Service(
      image: Option[String],
      container_name: Option[String],
      restart: Option[String],
      ports: Option[List[String]],
      command: Option[Command],         // Union Type
      environment: Option[Environment], // Union Type
      depends_on: Option[DependsOn],    // Union Type
      networks: Option[List[String]],
      volumes: Option[List[String]],
      logging: Option[Logging],
      healthcheck: Option[Healthcheck],
      deploy: Option[Deploy]
  ) derives YamlDecoder

  case class Network(driver: Option[String], internal: Option[Boolean]) derives YamlDecoder

  case class Volume(driver: Option[String]) derives YamlDecoder

  case class DockerCompose(
      version: Option[String],
      services: Map[String, Service],
      networks: Option[Map[String, Network]],
      volumes: Option[Map[String, Volume]]
  ) derives YamlDecoder
}
