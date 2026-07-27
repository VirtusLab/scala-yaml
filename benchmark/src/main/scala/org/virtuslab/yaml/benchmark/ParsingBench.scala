package org.virtuslab.yaml.benchmark

import org.virtuslab.yaml.*
import org.openjdk.jmh.annotations.{Benchmark, Param, Setup}
import org.virtuslab.yaml.benchmark.DockerCompose.DockerCompose
import org.virtuslab.yaml.benchmark.GeoYaml.GeoYaml
import org.virtuslab.yaml.internal.load.parse.{Event, ParserImpl}
import org.virtuslab.yaml.internal.load.reader.Tokenizer

import scala.compiletime.uninitialized
import scala.io.Source
import scala.util.Using

// To run: sbt 'benchmark/Jmh/run -prof gc ParsingBench'
class ParsingBench extends CommonParams {
  @Param(Array("docker-compose.yaml", "geo.yaml"))
  var name: String = uninitialized

  private var yaml: String = uninitialized

  @Setup
  def setup(): Unit =
    yaml = Using.resource(getClass.getResourceAsStream(s"/$name")) { in =>
      Source.fromInputStream(in, "UTF-8").mkString
    }

  @Benchmark
  def fromStringToEvents: Either[YamlError, List[Event]] =
    ParserImpl(Tokenizer.make(yaml)).getEvents()

  @Benchmark
  def fromStringToNode: Either[YamlError, Node] = parseYaml(yaml)

  @Benchmark
  def fromStringToStruct: Either[YamlError, Any] = name match {
    case "docker-compose.yaml" => decodeYaml[DockerCompose](yaml)
    case "geo.yaml"            => decodeYaml[GeoYaml](yaml)
  }
}
