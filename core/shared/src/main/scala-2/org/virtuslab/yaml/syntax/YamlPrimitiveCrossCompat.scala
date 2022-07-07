package org.virtuslab.yaml
package syntax

import Node._

private[yaml] trait YamlPrimitiveCompanionCrossCompat {
  implicit def fromString(s: String): YamlPrimitive   = YamlPrimitive(ScalarNode(s.toString))
  implicit def fromBoolean(s: Boolean): YamlPrimitive = YamlPrimitive(ScalarNode(s.toString))
  implicit def fromInt(s: Int): YamlPrimitive         = YamlPrimitive(ScalarNode(s.toString))
  implicit def fromLong(s: Long): YamlPrimitive       = YamlPrimitive(ScalarNode(s.toString))
  implicit def fromNode(s: Node): YamlPrimitive       = YamlPrimitive(s)
}
