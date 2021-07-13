package org.virtuslab.yaml

import org.virtuslab.internal.YamlError
import org.virtuslab.internal.load.compose.ComposerImpl
import org.virtuslab.internal.load.compose.Node
import org.virtuslab.yaml.YamlEncoder

import scala.deriving._
import org.virtuslab.internal.load.construct.Construct
import org.virtuslab.yaml

object Util:
  extension (str: String)
    def as[T](using c: Construct[T]): Either[YamlError, T] =
      for
        node <- ComposerImpl.compose(str)
        t    <- c.construct(node)
      yield t

  extension [T](t: T) def asYaml(using encoder: YamlEncoder[T]): String = encoder.toYaml(t)
