package org.virtuslab

import org.virtuslab.yaml.internal.dump.present.PresenterImpl
import org.virtuslab.yaml.internal.dump.serialize.SerializerImpl
import org.virtuslab.yaml.internal.load.compose.ComposerImpl
import org.virtuslab.yaml.internal.load.parse.Parser
import org.virtuslab.yaml.internal.load.parse.ParserImpl
import org.virtuslab.yaml.internal.load.reader.Tokenizer

package object yaml {

  /**
    * Parse YAML from the given string.
    */
  def parseYAML(str: String): Either[YamlError, Node] =
    for {
      events <- {
        val parser = ParserImpl(Tokenizer.make(str))
        parser.getEvents()
      }
      node <- ComposerImpl.fromEvents(events)
    } yield node

  /** Parse multiple YAML documents from the given string.
    */
  def parseAllYAMLs(str: String): Either[YamlError, List[Node]] =
    for {
      events <- {
        val parser = ParserImpl(Tokenizer.make(str))
        parser.getEvents()
      }
      nodes <- ComposerImpl.multipleFromEvents(events)
    } yield nodes

  implicit class StringOps(val str: String) extends AnyVal {

    /**
   * Parse YAML from the given [[String]], returning either [[YamlError]] or [[T]].
   *
   * According to the specification:
   * - [[Parser]] takes input string and produces sequence of events
   * - then [[Composer]] produces a representation graph from events
   * - finally [[YamlDecoder]] (construct phase from the YAML spec) constructs data type [[T]] from the YAML representation.
   */
    def as[T](implicit
        c: YamlDecoder[T],
        settings: LoadSettings = LoadSettings.empty
    ): Either[YamlError, T] =
      for {
        node <- parseYAML(str)
        t    <- node.as[T]
      } yield t

    def asNode: Either[YamlError, Node] = parseYAML(str)
  }

  implicit class AnyOps[T](val t: T) extends AnyVal {

    /**
   * Serialize a [[T]] into a YAML string.
   *
   * According to the specification:
   * - [[YamlEncoder]] encode type [[T]] into [[Node]]
   * - [[Serializer]] serializes [[Node]] into sequence of [[Event]]s
   * - [[Presenter]] present [[Events]] as a character stream
   */
    def asYaml(implicit encoder: YamlEncoder[T]): String = {
      val node = encoder.asNode(t)
      node.asYaml
    }
  }

  implicit class NodeOps(val node: Node) extends AnyVal {
    def asYaml: String = {
      val events = SerializerImpl.toEvents(node)
      PresenterImpl.asString(events)
    }
  }

}
