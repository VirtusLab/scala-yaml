package org.virtuslab.yaml.syntax

import scala.language.implicitConversions

import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Node.ScalarNode

final case class YamlPrimitive(node: Node)
object YamlPrimitive extends YamlPrimitiveCompanionCrossCompat
