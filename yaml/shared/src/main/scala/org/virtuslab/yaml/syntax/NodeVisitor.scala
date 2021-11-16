package org.virtuslab.yaml.syntax

import org.virtuslab.yaml.{Node, Range, TraverseError}
import org.virtuslab.yaml.Node.*
import org.virtuslab.yaml.syntax.NodeSelector.*

case class NodeVisitor(node: Node, selectors: List[NodeSelector]):
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
      nodes: Seq[Node],
      pos: Option[Range]
  ): Either[TraverseError, SequenceNode] =
    selectors match {
      case IntSelector(index) :: rest =>
        val updateNode = NodeVisitor(nodes(index), rest).setValue(value)
        updateNode.map(node => new SequenceNode(nodes.updated(index, node), pos))

      case StringSelector(field) :: rest =>
        Left(
          TraverseError(
            s"Expeceted index of sequence, insted found string path: ${field}"
          )
        )
      case _ => Left(TraverseError(s"Expeceted index of sequence, insted found end of path"))
    }

  def updateMappingNode(value: String, mappings: Seq[KeyValueNode], pos: Option[Range]) = {
    selectors match {
      case StringSelector(field) :: rest =>
        val updatedKeyIndex = mappings.indexWhere {
          case KeyValueNode(ScalarNode(keyName, _), _, _) => keyName == field
          case _                                          => false
        }

        if (updatedKeyIndex >= 0) {
          val updatedKey = mappings(updatedKeyIndex) match {
            case KeyValueNode(ScalarNode(keyName, scalarPos), valueNode, pos) =>
              val updatedNode = NodeVisitor(valueNode, rest).setValue(value)
              updatedNode.map { node =>
                KeyValueNode(
                  ScalarNode(keyName, scalarPos),
                  node,
                  pos
                )
              }
            case _ => Left(TraverseError(s"Not found $field in mapping"))
          }
          updatedKey.map { key =>
            new MappingNode(mappings.updated(updatedKeyIndex, key), pos)
          }
        } else {
          Left(TraverseError(s"Not found $field in mapping"))
        }
      case IntSelector(index) :: rest =>
        Left(
          TraverseError(
            s"Expeceted plain test, insted found index: $index"
          )
        )
      case _ => Left(TraverseError(s"Expeceted plain text, insted found end of path"))
    }
  }

  def setValue(value: String): Either[TraverseError, Node] = node match {
    case scalar: ScalarNode         => updateScalarNode(value, scalar)
    case SequenceNode(nodes, pos)   => updateSequenceNode(value, nodes, pos)
    case MappingNode(mappings, pos) => updateMappingNode(value, mappings, pos)
  }

object NodeVisitor
