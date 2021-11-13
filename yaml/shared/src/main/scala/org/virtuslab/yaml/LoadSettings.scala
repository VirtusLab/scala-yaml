package org.virtuslab.yaml

case class LoadSettings(constructors: Map[Tag, YamlDecoder[Any]])
object LoadSettings:
  val empty = LoadSettings(Map.empty)
