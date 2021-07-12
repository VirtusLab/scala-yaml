package org.virtuslab.internal.load.construct

import scala.util.Try
trait Der[T]:
  def read(input: String): Either[Throwable, T]

object Der:
  given Der[Int]    = (input) => Try(input.toInt).toEither
  given Der[String] = (input) => Right(input)
  given Der[Double] = (input) => Try(input.toDouble).toEither
