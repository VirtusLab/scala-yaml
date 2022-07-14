package org.virtuslab.yaml

import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Node._
import org.virtuslab.yaml.Range
import org.virtuslab.yaml.ModifyError
import org.virtuslab.yaml.YamlError
import org.virtuslab.yaml.syntax.NodeSelector
import org.virtuslab.yaml.syntax.NodeSelector._

class NodeVisitor(node: Node, selectors: List[NodeSelector]) {
  def apply(index: Int): NodeVisitor =
    NodeVisitor(node, selectors :+ NodeSelector.IntSelector(index))
  def apply(field: String): NodeVisitor =
    NodeVisitor(node, selectors :+ NodeSelector.StringSelector(field))

  private def updateScalarNode(
      modifyValue: String => String,
      scalar: ScalarNode
  ): Either[ModifyError, ScalarNode] =
    selectors match {
      case Nil =>
        Right(
          scalar.copy(
            value = modifyValue(scalar.value)
          )
        )
      case _ =>
        Left(
          ModifyError(
            s"Expected end of scalar path, instead found path ${selectors.map(_.show).mkString(".")}"
          )
        )
    }

  private def removeScalarNode(scalar: ScalarNode): Either[ModifyError, ScalarNode] =
    selectors match {
      case Nil =>
        Right(
          scalar.copy(
            value = "",
            tag = Tag.nullTag
          )
        )
      case _ =>
        Left(
          ModifyError(
            s"Expected end of scalar path, instead found path ${selectors.map(_.show).mkString(".")}"
          )
        )
    }

  private def updateSequenceNode(
      modifyValue: String => String,
      sequence: SequenceNode
  ): Either[ModifyError, SequenceNode] =
    selectors match {
      case IntSelector(index) :: rest =>
        val nodes      = sequence.nodes
        val updateNode = NodeVisitor(nodes(index), rest).modifyValue(modifyValue)
        updateNode.map(node => sequence.copy(nodes = nodes.updated(index, node)))

      case StringSelector(field) :: rest =>
        Left(
          ModifyError(
            s"Found string path: ${field} traversing sequence, but index was expected"
          )
        )
      case _ => Left(ModifyError(s"Found end of path, but index was expected"))
    }

  private def removeSequenceNode(
      sequence: SequenceNode
  ): Either[ModifyError, SequenceNode] =
    selectors match {
      case IntSelector(index) :: rest =>
        if (rest.isEmpty) {
          Right(
            sequence.copy(
              nodes = sequence.nodes.patch(index, Nil, 1)
            )
          )
        } else {
          val nodes      = sequence.nodes
          val updateNode = NodeVisitor(nodes(index), rest).removeValue()
          updateNode.map(node => sequence.copy(nodes = nodes.updated(index, node)))
        }
      case StringSelector(field) :: rest =>
        Left(
          ModifyError(
            s"Expected index of sequence, instead found string path: ${field}"
          )
        )
      case _ => Left(ModifyError(s"Expected index of sequence, instead found end of path"))
    }

  private def updateMappingNode(modifyValue: String => String, mapping: MappingNode) =
    selectors match {
      case StringSelector(field) :: rest =>
        val mappings = mapping.mappings
        val entryToUpdateOpt = mappings.find {
          case (ScalarNode(keyName, _), _) => keyName == field
          case _                           => false
        }

        entryToUpdateOpt match {
          case Some(entryToUpdate) =>
            val updatedValueE = entryToUpdate match {
              case (ScalarNode(keyName, _), valueNode) =>
                val updatedNode = NodeVisitor(valueNode, rest).modifyValue(modifyValue)
                updatedNode
              case _ => Left(ModifyError(s"Not found $field in mapping"))
            }

            updatedValueE.map { updatedValue =>
              mapping.copy(
                mappings.updated(entryToUpdate._1, updatedValue)
              )
            }
          case None => Left(ModifyError(s"Not found $field in mapping"))
        }
      case IntSelector(index) :: rest =>
        Left(
          ModifyError(
            s"Expected plain test, instead found index: $index"
          )
        )
      case _ => Left(ModifyError(s"Expected plain text, instead found end of path"))
    }

  private def removeMappingNode(mapping: MappingNode) =
    selectors match {
      case StringSelector(field) :: rest =>
        if (rest.isEmpty) {
          Right(
            mapping.copy(
              mappings = mapping.mappings.filter {
                case (ScalarNode(keyName, _), _) => keyName != field
                case _                           => false
              }
            )
          )
        } else {
          val mappings = mapping.mappings
          val entryToUpdateOpt = mappings.find {
            case (ScalarNode(keyName, _), _) => keyName == field
            case _                           => false
          }

          entryToUpdateOpt match {
            case Some(entryToUpdate) =>
              val updatedValueE = entryToUpdate match {
                case (ScalarNode(keyName, _), valueNode) =>
                  val updatedNode = NodeVisitor(valueNode, rest).removeValue()
                  updatedNode
                case _ => Left(ModifyError(s"Not found $field in mapping"))
              }

              updatedValueE.map { updatedValue =>
                mapping.copy(
                  mappings.updated(entryToUpdate._1, updatedValue)
                )
              }
            case None => Left(ModifyError(s"Not found $field in mapping"))
          }
        }
      case IntSelector(index) :: rest =>
        Left(
          ModifyError(
            s"Expected plain test, instead found index: $index"
          )
        )
      case _ => Left(ModifyError(s"Expected plain text, instead found end of path"))
    }

  def modifyValue(fn: String => String): Either[ModifyError, Node] = node match {
    case scalar: ScalarNode     => updateScalarNode(fn, scalar)
    case sequence: SequenceNode => updateSequenceNode(fn, sequence)
    case mapping: MappingNode   => updateMappingNode(fn, mapping)
  }

  def setValue(value: String): Either[ModifyError, Node] = node match {
    case scalar: ScalarNode     => updateScalarNode((_) => value, scalar)
    case sequence: SequenceNode => updateSequenceNode((_) => value, sequence)
    case mapping: MappingNode   => updateMappingNode((_) => value, mapping)
  }

  def removeValue(): Either[ModifyError, Node] = node match {
    case scalar: ScalarNode     => removeScalarNode(scalar)
    case sequence: SequenceNode => removeSequenceNode(sequence)
    case mapping: MappingNode   => removeMappingNode(mapping)
  }
}

object NodeVisitor {

  def apply(node: Node, selectors: List[NodeSelector]): NodeVisitor =
    new NodeVisitor(node, selectors)

  implicit class EitherOps(val either: Either[ModifyError, NodeVisitor]) extends AnyVal {
    def apply(field: String): Either[ModifyError, NodeVisitor] = either.map(_.apply(field))

    def apply(index: Int): Either[ModifyError, NodeVisitor] = either.map(_.apply(index))

    def setValue(value: String): Either[ModifyError, Node] =
      either.flatMap(_.modifyValue((_) => value))

    def modifyValue(fn: String => String): Either[ModifyError, Node] =
      either.flatMap(_.modifyValue(fn))

    def removeValue(): Either[ModifyError, Node] = either.flatMap(_.removeValue())
  }
}
