package org.virtuslab.yaml

import scala.deriving._
import scala.compiletime._
import org.virtuslab.internal.dump.YamlWriter
import org.virtuslab.internal.load.compose.Node

trait YamlEncoder[T: Mirror.Of]:

  given yamlWriter: YamlWriter[T] = YamlWriter.derived[T]

  final def toYaml(t: T): String =
    yamlWriter.toYaml(t)

  def apply(obj: T): Node = ???

object YamlEncoder:
  inline def derived[T](using m: Mirror.Of[T]): YamlEncoder[T] = {
    new YamlEncoder[T] {}
  }
