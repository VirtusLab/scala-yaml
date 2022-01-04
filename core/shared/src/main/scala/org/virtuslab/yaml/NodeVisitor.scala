package org.virtuslab.yaml

import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Node.*
import org.virtuslab.yaml.Range
import org.virtuslab.yaml.TraverseError
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.syntax.NodeSelector
import org.virtuslab.yaml.syntax.NodeSelector.*

class NodeVisitor(node: Node, selectors: List[NodeSelector]):
  def apply(index: Int): NodeVisitor =
    NodeVisitor(node, selectors :+ NodeSelector.IntSelector(index))
  def apply(field: String): NodeVisitor =
    NodeVisitor(node, selectors :+ NodeSelector.StringSelector(field))

  private def updateScalarNode(
      value: String,
      scalar: ScalarNode
  ): Either[TraverseError, ScalarNode] =
    selectors match {
      case Nil =>
        Right(
          scalar.copy(
            value = value
          )
        )
      case _ =>
        Left(
          TraverseError(
            s"Expected end of scalar path, instead found path ${selectors.map(_.show).mkString(".")}"
          )
        )
    }

  private def updateSequenceNode(
      value: String,
      sequence: SequenceNode
  ): Either[TraverseError, SequenceNode] =
    selectors match {
      case IntSelector(index) :: rest =>
        val nodes      = sequence.nodes
        val updateNode = NodeVisitor(nodes(index), rest).setValue(value)
        updateNode.map(node => sequence.copy(nodes = nodes.updated(index, node)))

      case StringSelector(field) :: rest =>
        Left(
          TraverseError(
            s"Expeceted index of sequence, insted found string path: ${field}"
          )
        )
      case _ => Left(TraverseError(s"Expeceted index of sequence, insted found end of path"))
    }

  private def updateMappingNode(value: String, mapping: MappingNode) =
    selectors match
      case StringSelector(field) :: rest =>
        val mappings = mapping.mappings
        val entryToUpdateOpt = mappings.find {
          case (ScalarNode(keyName, _), _) => keyName == field
          case _                           => false
        }

        entryToUpdateOpt match
          case Some(entryToUpdate) =>
            val updatedValueE = entryToUpdate match
              case (ScalarNode(keyName, _), valueNode) =>
                val updatedNode = NodeVisitor(valueNode, rest).setValue(value)
                updatedNode
              case _ => Left(TraverseError(s"Not found $field in mapping"))

            updatedValueE.map { updatedValue =>
              mapping.copy(
                mappings.updated(entryToUpdate._1, updatedValue)
              )
            }
          case None => Left(TraverseError(s"Not found $field in mapping"))
      case IntSelector(index) :: rest =>
        Left(
          TraverseError(
            s"Expeceted plain test, insted found index: $index"
          )
        )
      case _ => Left(TraverseError(s"Expeceted plain text, insted found end of path"))

  def setValue(value: String): Either[TraverseError, Node] = node match
    case scalar: ScalarNode     => updateScalarNode(value, scalar)
    case sequence: SequenceNode => updateSequenceNode(value, sequence)
    case mapping: MappingNode   => updateMappingNode(value, mapping)

object NodeVisitor:

  def apply(node: Node, selectors: List[NodeSelector]): NodeVisitor =
    new NodeVisitor(node, selectors)

  extension (either: Either[TraverseError, NodeVisitor])
    def apply(field: String): Either[TraverseError, NodeVisitor] = either.map(_.apply(field))

  extension (either: Either[TraverseError, NodeVisitor])
    def apply(index: Int): Either[TraverseError, NodeVisitor] = either.map(_.apply(index))

  extension (either: Either[TraverseError, NodeVisitor])
    def setValue(value: String): Either[TraverseError, Node] = either.flatMap(_.setValue(value))
