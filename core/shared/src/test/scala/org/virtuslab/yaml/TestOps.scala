package org.virtuslab.yaml

object TestOps {

  implicit class EitherOps[E <: YamlError, T](val either: Either[E, T]) {
    def orThrow: T =
      either match {
        case Left(e)  => throw new RuntimeException(e.msg)
        case Right(t) => t
      }
  }

}
