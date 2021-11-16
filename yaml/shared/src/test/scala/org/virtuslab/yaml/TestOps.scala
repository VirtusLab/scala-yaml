package org.virtuslab.yaml

object TestOps {

  extension [E <: YamlError, T](either: Either[E, T])
    def orThrow: T =
      either match
        case Left(e)  => throw new RuntimeException(e.msg)
        case Right(t) => t

}
