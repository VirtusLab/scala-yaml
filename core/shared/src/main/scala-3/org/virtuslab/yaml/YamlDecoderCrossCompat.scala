package org.virtuslab.yaml

import org.virtuslab.yaml.Node.*

import scala.compiletime.*
import scala.quoted.*
import scala.deriving.Mirror

private[yaml] trait YamlDecoderCompanionCrossCompat extends DecoderMacros {
  inline def derived[T](using m: Mirror.Of[T]): YamlDecoder[T] = inline m match
    case p: Mirror.ProductOf[T] => deriveProduct(p)
    case s: Mirror.SumOf[T]     => sumOf(s)
}

private[yaml] trait DecoderMacros {

  protected inline def deriveProduct[T](p: Mirror.ProductOf[T]) = ${
    DecoderMacros.deriveProductImpl[T]('p)
  }

  protected inline def sumOf[T](s: Mirror.SumOf[T]) =
    val instances = summonSumOf[s.MirroredElemTypes].asInstanceOf[List[YamlDecoder[T]]]
    new YamlDecoder[T]:
      override def construct(
          node: Node
      )(using constructor: LoadSettings = LoadSettings.empty): Either[ConstructError, T] = LazyList
        .from(instances)
        .map(c => c.construct(node))
        .collectFirst { case r @ Right(_) => r }
        .getOrElse(Left(ConstructError.from(s"Cannot parse $node", node)))

  protected inline def summonSumOf[T <: Tuple]: List[YamlDecoder[_]] = inline erasedValue[T] match
    case _: (t *: ts) =>
      summonFrom { case p: Mirror.ProductOf[`t`] =>
        deriveProduct(p) :: summonSumOf[ts]
      }
    case _: EmptyTuple => Nil

}

object DecoderMacros {

  protected def constructValues[T](
      instances: List[(String, YamlDecoder[?], Boolean)],
      valuesMap: Map[String, Node],
      defaultParams: Map[String, Any],
      p: Mirror.ProductOf[T],
      parentNode: Node
  ): Either[ConstructError, T] = {
    val values = instances.map { case (label, c, isOptional) =>
      valuesMap.get(label) match
        case Some(value) => c.construct(value)
        case None =>
          if (isOptional) Right(None)
          else if (defaultParams.contains(label)) Right(defaultParams(label))
          else Left(ConstructError.from(s"Key $label doesn't exist in parsed document", parentNode))
    }
    val (left, right) = values.partitionMap(identity)
    if left.nonEmpty then Left(left.head)
    else Right(p.fromProduct(Tuple.fromArray(right.toArray)))
  }

  private def extractKeyValues(
      mappings: Map[Node, Node]
  ): Either[ConstructError, Map[String, Node]] = {
    val keyValueMap = mappings
      .map { (k, v) =>
        k match {
          case ScalarNode(scalarKey, _) => Right((scalarKey, v))
          case node =>
            Left(ConstructError.from(s"Parameter of a class must be a scalar value", node))
        }
      }
    val (error, valuesSeq) = keyValueMap.partitionMap(identity)

    if (error.nonEmpty) Left(error.head)
    else Right(valuesSeq.toMap)
  }

  def deriveProductImpl[T: Type](p: Expr[Mirror.ProductOf[T]])(using
      Quotes
  ): Expr[YamlDecoder[T]] =

    // returns a list of tuples of label, instance, isOptional
    def prepareInstances(
        elemLabels: Type[?],
        elemTypes: Type[?]
    ): List[Expr[(String, YamlDecoder[?], Boolean)]] =
      (elemLabels, elemTypes) match
        case ('[EmptyTuple], '[EmptyTuple]) => Nil
        case ('[label *: labelsTail], '[tpe *: tpesTail]) =>
          val label = Type.valueOfConstant[label].get.asInstanceOf[String]
          val isOption = Type.of[tpe] match
            case '[Option[?]] => Expr(true)
            case _            => Expr(false)

          val fieldName = Expr(label)
          val fieldFormat = Expr.summon[YamlDecoder[tpe]].getOrElse {
            quotes.reflect.report
              .errorAndAbort("Missing given instance of YamlDecoder[" ++ Type.show[tpe] ++ "]")
          }
          val namedInstance = '{ (${ fieldName }, $fieldFormat, ${ isOption }) }
          namedInstance :: prepareInstances(Type.of[labelsTail], Type.of[tpesTail])

    p match
      case '{
            $m: Mirror.ProductOf[T] {
              type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes
            }
          } =>
        val allInstancesExpr =
          Expr.ofList(prepareInstances(Type.of[elementLabels], Type.of[elementTypes]))
        val defaultParamsExpr = findDefaultParams[T]

        '{
          new YamlDecoder[T] {
            private val allInstances  = $allInstancesExpr
            private val defaultParams = $defaultParamsExpr
            private val mirror        = $p

            override def construct(node: Node)(using
                constructor: LoadSettings = LoadSettings.empty
            ): Either[ConstructError, T] =
              node match
                case Node.MappingNode(mappings, _) =>
                  for {
                    valuesMap <- extractKeyValues(mappings)
                    constructedValues <- constructValues(
                      allInstances,
                      valuesMap,
                      defaultParams,
                      mirror,
                      node
                    )
                  } yield (constructedValues)
                case _ =>
                  Left(
                    ConstructError.from(
                      s"Expected MappingNode, got ${node.getClass.getSimpleName}",
                      node
                    )
                  )
          }
        }

  protected def findDefaultParams[T](using quotes: Quotes, tpe: Type[T]): Expr[Map[String, Any]] =
    import quotes.reflect.*

    TypeRepr.of[T].classSymbol match
      case None => '{ Map.empty[String, Any] }
      case Some(sym) =>
        val comp = sym.companionClass
        try
          val mod = Ref(sym.companionModule)
          val names =
            for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
            yield p.name
          val namesExpr: Expr[List[String]] =
            Expr.ofList(names.map(Expr(_)))

          val body = comp.tree.asInstanceOf[ClassDef].body
          val idents: List[Ref] =
            for
              case deff @ DefDef(name, _, _, _) <- body
              if name.startsWith("$lessinit$greater$default")
            yield mod.select(deff.symbol)
          val typeArgs = TypeRepr.of[T].typeArgs
          val identsExpr: Expr[List[Any]] =
            if typeArgs.isEmpty then Expr.ofList(idents.map(_.asExpr))
            else Expr.ofList(idents.map(_.appliedToTypes(typeArgs).asExpr))

          '{ $namesExpr.zip($identsExpr).toMap }
        catch // TODO drop after https://github.com/lampepfl/dotty/issues/19732 (after bump to 3.3.4)
          case cce: ClassCastException =>
            '{
              Map.empty[String, Any]
            }
}
