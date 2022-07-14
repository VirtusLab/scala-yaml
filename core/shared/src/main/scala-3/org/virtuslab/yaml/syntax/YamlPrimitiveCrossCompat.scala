package org.virtuslab.yaml
package syntax

import org.virtuslab.yaml.Node.*

private[yaml] trait YamlPrimitiveCompanionCrossCompat {
  given Conversion[String, YamlPrimitive]  = s => YamlPrimitive(ScalarNode(s))
  given Conversion[Boolean, YamlPrimitive] = s => YamlPrimitive(ScalarNode(s.toString))
  given Conversion[Int, YamlPrimitive]     = s => YamlPrimitive(ScalarNode(s.toString))
  given Conversion[Long, YamlPrimitive]    = s => YamlPrimitive(ScalarNode(s.toString))
  given Conversion[Node, YamlPrimitive]    = s => YamlPrimitive(s)

  given conv[A, B](using
      c1: Conversion[A, YamlPrimitive],
      c2: Conversion[B, YamlPrimitive]
  ): Conversion[(A, B), (YamlPrimitive, YamlPrimitive)] = (s, b) => (c1(s), c2(b))

  given singleConv[A, B](using
      conversion: Conversion[A, YamlPrimitive]
  ): Conversion[(A, Node), (YamlPrimitive, YamlPrimitive)] = (s, b) =>
    (conversion(s), YamlPrimitive(b))
}
