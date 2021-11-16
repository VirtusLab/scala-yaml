package org.virtuslab.yaml

object TestOps {

  implicit class EitherThrowOps[E <: YamlError, T](private val either: Either[E, T])
      extends AnyVal {
    def orThrow: T =
      either match {
        case Left(e)  => throw new RuntimeException(e.msg)
        case Right(t) => t
      }
  }

}
