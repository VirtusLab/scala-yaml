package org.virtuslab.yaml

case class LoadSettings(constructors: Map[Tag, YamlDecoder[_]])
object LoadSettings {
  val empty = LoadSettings(Map.empty)
}
