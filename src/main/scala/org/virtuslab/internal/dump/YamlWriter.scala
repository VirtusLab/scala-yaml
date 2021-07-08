package org.virtuslab.internal.dump

import scala.deriving._
import scala.compiletime._
import scala.deriving.Mirror

trait YamlWriter[T]:
  def toYaml(obj: T): String

object YamlWriter:
  inline def derived[T](using m: Mirror.Of[T]): YamlWriter[T] = {
    new YamlWriter[T] {
      override def toYaml(obj: T): String = ???
    }
  }
