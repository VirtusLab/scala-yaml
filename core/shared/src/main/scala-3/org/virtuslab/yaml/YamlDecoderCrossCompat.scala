package org.virtuslab.yaml

import org.virtuslab.yaml.Node.*

import scala.compiletime.*
import scala.quoted.*
import scala.deriving.Mirror

private[yaml] trait YamlDecoderCompanionCrossCompat {
  inline def derived[T](using m: Mirror.Of[T]): YamlDecoder[T] = ${ DecoderMacros.derivedImpl('m) }
}

object DecoderMacros {

  def derivedImpl[T: Type](m: Expr[Mirror.Of[T]])(using Quotes): Expr[YamlDecoder[T]] =
    m match
      case '{ $m: Mirror.ProductOf[T] } => deriveProduct(m)
      case '{ $m: Mirror.SumOf[T] }     => deriveSum(m)

  protected def summonSumOf[T <: Tuple: Type](using q: Quotes): List[Expr[YamlDecoder[_]]] =
    import q.reflect.report.*
    Type.of[T] match
      case '[t *: ts] =>
        Expr.summon[Mirror.ProductOf[t]] match
          case Some(p) => deriveProduct[t](p) :: summonSumOf[ts]
          case None =>
            Expr.summon[YamlDecoder[t]] match
              case Some(d) => d :: summonSumOf[ts]
              case None => errorAndAbort(s"Missing given instance of YamlDecoder[${Type.show[t]}]")
      case '[EmptyTuple] => Nil

  def deriveSum[T: Type](s: Expr[Mirror.SumOf[T]])(using Quotes): Expr[YamlDecoder[T]] =
    s match
      case '{
            type elementTypes <: Tuple;
            $m: Mirror.SumOf[T] { type MirroredElemTypes = `elementTypes` }
          } =>
        val instancesExpr = Expr.ofList(summonSumOf[elementTypes])
        '{
          new YamlDecoder[T] {
            private val instances = $instancesExpr.asInstanceOf[List[YamlDecoder[T]]]
            override def construct(node: Node)(using
                constructor: LoadSettings = LoadSettings.empty
            ): Either[ConstructError, T] =
              instances
                .map(_.construct(node))
                .collectFirst { case r @ Right(_) => r }
                .getOrElse(
                  Left(ConstructError.from(s"Cannot parse $node", node))
                )
          }
        }

  protected def constructValues[T](
      instances: List[(String, YamlDecoder[?], Boolean)],
      valuesMap: Map[String, Node],
      defaultParams: Map[String, () => Any],
      p: Mirror.ProductOf[T],
      parentNode: Node
  ): Either[ConstructError, T] = {
    val values = instances.map { case (label, c, isOptional) =>
      valuesMap.get(label) match
        case Some(value) => c.construct(value)
        case None =>
          if (isOptional) Right(None)
          else if (defaultParams.contains(label))
            val defaultParamCreator = defaultParams(label)
            val defaultParamValue   = defaultParamCreator()
            Right(defaultParamValue)
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

  def deriveProduct[T: Type](p: Expr[Mirror.ProductOf[T]])(using
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

  private val DefaultParamPrefix = "$lessinit$greater$default$"

  protected def findDefaultParams[T](using
      quotes: Quotes,
      tpe: Type[T]
  ): Expr[Map[String, () => Any]] =
    import quotes.reflect.*

    TypeRepr.of[T].classSymbol match
      case None => '{ Map.empty[String, () => Any] }
      case Some(sym: Symbol) =>
        try
          val comp = sym.companionClass
          val mod  = Ref(sym.companionModule)
          val paramWithDefaultMeta =
            for (p, idx) <- sym.caseFields.zipWithIndex if p.flags.is(Flags.HasDefault)
            // +1 because the names are generated starting from 1
            yield (p.name, idx + 1)

          val idents: List[(String, Ref)] =
            for (paramName, idx) <- paramWithDefaultMeta
            yield paramName -> mod.select(
              // head is safe here because we know there has to be a getter for the default value
              // because we checked for HasDefault flag
              comp.methodMember(DefaultParamPrefix + idx.toString).head
            )

          val typeArgs = TypeRepr.of[T].typeArgs

          // we create an expression of a list of tuples of name and thunks that return the default value for a given parameter
          val defaultsThunksExpr: Expr[List[(String, () => Any)]] =
            if typeArgs.isEmpty then
              Expr.ofList(
                idents.map { case (name, ref) => name -> ref.asExpr }.map { case (name, '{ $x }) =>
                  '{ (${ Expr(name) }, () => $x) }
                }
              )
            else // if there are type parameters, we need to apply the type parameters to accessors
              Expr.ofList(
                idents.map { case (name, ref) => name -> ref.appliedToTypes(typeArgs).asExpr }.map {
                  case (name, '{ $x }) => '{ (${ Expr(name) }, () => $x) }
                }
              )

          '{ $defaultsThunksExpr.toMap }
        catch // TODO drop after https://github.com/lampepfl/dotty/issues/19732 (after bump to 3.3.4)
          case cce: ClassCastException =>
            '{
              Map.empty[String, () => Any]
            }
}
