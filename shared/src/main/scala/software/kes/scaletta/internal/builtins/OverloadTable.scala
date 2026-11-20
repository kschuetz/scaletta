package software.kes.scaletta.internal.builtins

import software.kes.scaletta.api.{ProperType, TypeId}
import software.kes.scaletta.internal.symbols.{SignatureQuery, SignatureQueryParameter}
import software.kes.scaletta.internal.types.{TypeConversion, TypeConversionGraph, TypeUniverse}

object OverloadTable {
  /**
   * Disambiguates an applicable candidate set by finding the unique most-specific definition.
   */
  def resolveBestMatch(typeUniverse: TypeUniverse,
                       candidates: Vector[NativeFunctionDefinition]): Either[ResolutionError, NativeFunctionDefinition] = {
    if (candidates.isEmpty) {
      Left(ResolutionError.NotFound)
    } else if (candidates.size == 1) {
      Right(candidates.head)
    } else {
      val bests = candidates.filter { c1 =>
        !candidates.exists(c2 => c1 != c2 && isMoreSpecific(typeUniverse, c2, c1))
      }

      val distinctBests = bests.distinct
      if (distinctBests.isEmpty) {
        Left(ResolutionError.NotFound)
      } else if (distinctBests.size == 1) {
        Right(distinctBests.head)
      } else {
        Left(ResolutionError.Ambiguous)
      }
    }
  }

  def resolveBestMatch(typeUniverse: TypeUniverse,
                       candidates: List[NativeFunctionDefinition]): Either[ResolutionError, NativeFunctionDefinition] =
    resolveBestMatch(typeUniverse, candidates.toVector)

  /**
   * Resolves a static or local function call site given candidate definitions and argument types.
   */
  def resolveFunctionCall(typeUniverse: TypeUniverse,
                          candidates: Vector[NativeFunctionDefinition],
                          argTypes: Vector[Vector[ProperType[TypeId]]]): Either[ResolutionError, NativeFunctionDefinition] = {
    val exactMatches = candidates.filter(c => isApplicable(typeUniverse, c, argTypes))
    if (exactMatches.nonEmpty) {
      resolveBestMatch(typeUniverse, exactMatches)
    } else {
      val conversionMatches = candidates.filter(c => isApplicableWithConversions(typeUniverse, c, argTypes))
      if (conversionMatches.isEmpty) {
        Left(ResolutionError.NotFound)
      } else {
        val withCosts = conversionMatches.map(c => (c, calculateTotalCost(typeUniverse, c, argTypes)))
        val minCost = withCosts.map(_._2).min
        val bestByCost = withCosts.filter(_._2 == minCost).map(_._1)
        resolveBestMatch(typeUniverse, bestByCost)
      }
    }
  }

  /**
   * Resolves an instance method call site given candidate definitions, receiver type, and argument types.
   */
  def resolveMethodCall(typeUniverse: TypeUniverse,
                        candidates: Vector[NativeFunctionDefinition],
                        receiverType: ProperType[TypeId],
                        argTypes: Vector[Vector[ProperType[TypeId]]]): Either[ResolutionError, NativeFunctionDefinition] = {
    val effectiveArgTypes = if (argTypes.isEmpty) {
      Vector(Vector(receiverType))
    } else {
      (receiverType +: argTypes.head) +: argTypes.tail
    }
    resolveFunctionCall(typeUniverse, candidates, effectiveArgTypes)
  }

  private def isApplicable(typeUniverse: TypeUniverse,
                           definition: NativeFunctionDefinition,
                           argTypes: Vector[Vector[ProperType[TypeId]]]): Boolean = {
    if (definition.paramGroups.size != argTypes.size) {
      false
    } else {
      definition.paramGroups.zip(argTypes).forall { case (formalGroup, argGroup) =>
        formalGroup.variadic match {
          case Some(v) =>
            argGroup.size >= formalGroup.params.size && {
              val (fixedArgs, varArgs) = argGroup.splitAt(formalGroup.params.size)
              formalGroup.params.zip(fixedArgs).forall { case (param, arg) =>
                typeUniverse.hierarchy.isSubtype(arg, param.typ)
              } && varArgs.forall(arg => typeUniverse.hierarchy.isSubtype(arg, v.typ))
            }
          case None =>
            formalGroup.params.size == argGroup.size &&
              formalGroup.params.zip(argGroup).forall { case (param, arg) =>
                typeUniverse.hierarchy.isSubtype(arg, param.typ)
              }
        }
      }
    }
  }

  private def isApplicableWithConversions(typeUniverse: TypeUniverse,
                                          definition: NativeFunctionDefinition,
                                          argTypes: Vector[Vector[ProperType[TypeId]]]): Boolean = {
    if (definition.paramGroups.size != argTypes.size) {
      false
    } else {
      definition.paramGroups.zip(argTypes).forall { case (formalGroup, argGroup) =>
        formalGroup.variadic match {
          case Some(v) =>
            argGroup.size >= formalGroup.params.size && {
              val (fixedArgs, varArgs) = argGroup.splitAt(formalGroup.params.size)
              formalGroup.params.zip(fixedArgs).forall { case (param, arg) =>
                typeUniverse.hierarchy.isSubtype(arg, param.typ) ||
                  TypeConversionGraph.conversionRelation(arg, param.typ) != TypeConversion.None
              } && varArgs.forall { arg =>
                typeUniverse.hierarchy.isSubtype(arg, v.typ) ||
                  TypeConversionGraph.conversionRelation(arg, v.typ) != TypeConversion.None
              }
            }
          case None =>
            formalGroup.params.size == argGroup.size &&
              formalGroup.params.zip(argGroup).forall { case (param, arg) =>
                typeUniverse.hierarchy.isSubtype(arg, param.typ) ||
                  TypeConversionGraph.conversionRelation(arg, param.typ) != TypeConversion.None
              }
        }
      }
    }
  }

  private def calculateTotalCost(typeUniverse: TypeUniverse,
                                 definition: NativeFunctionDefinition,
                                 argTypes: Vector[Vector[ProperType[TypeId]]]): Int = {
    definition.paramGroups.zip(argTypes).foldLeft(0) { case (acc, (formalGroup, argGroup)) =>
      formalGroup.variadic match {
        case Some(v) =>
          val (fixedArgs, varArgs) = argGroup.splitAt(formalGroup.params.size)
          val fixedCost = formalGroup.params.zip(fixedArgs).foldLeft(0) { case (acc2, (param, arg)) =>
            val cost = if (typeUniverse.hierarchy.isSubtype(arg, param.typ)) {
              0
            } else {
              TypeConversionGraph.conversionRelation(arg, param.typ).cost
            }
            acc2 + cost
          }
          val varCost = varArgs.foldLeft(0) { case (acc2, arg) =>
            val cost = if (typeUniverse.hierarchy.isSubtype(arg, v.typ)) {
              0
            } else {
              TypeConversionGraph.conversionRelation(arg, v.typ).cost
            }
            acc2 + cost
          }
          acc + fixedCost + varCost
        case None =>
          val groupCost = formalGroup.params.zip(argGroup).foldLeft(0) { case (acc2, (param, arg)) =>
            val cost = if (typeUniverse.hierarchy.isSubtype(arg, param.typ)) {
              0
            } else {
              TypeConversionGraph.conversionRelation(arg, param.typ).cost
            }
            acc2 + cost
          }
          acc + groupCost
      }
    }
  }

  def isMoreSpecific(typeUniverse: TypeUniverse,
                     v1: NativeFunctionDefinition,
                     v2: NativeFunctionDefinition): Boolean = {
    if (v1 == v2) {
      false
    } else if (v1.paramGroups.size != v2.paramGroups.size) {
      false
    } else {
      final case class Acc(compatible: Boolean, hasAdvantage: Boolean)

      val result = v1.paramGroups.zip(v2.paramGroups).foldLeft(Acc(compatible = true, hasAdvantage = false)) {
        case (acc, (g1, g2)) =>
          if (!acc.compatible) {
            acc
          } else {
            (g1.variadic, g2.variadic) match {
              case (None, None) =>
                val compatible = g1.params.size == g2.params.size &&
                  g1.params.zip(g2.params).forall { case (p1, p2) =>
                    typeUniverse.hierarchy.isSubtype(p1.typ, p2.typ)
                  }
                val advantage = compatible && g1.params.zip(g2.params).exists { case (p1, p2) =>
                  p1.typ != p2.typ
                }
                Acc(compatible, acc.hasAdvantage || advantage)

              case (None, Some(v2Var)) =>
                val compatible = g1.params.size >= g2.params.size && {
                  val (prefix, suffix) = g1.params.splitAt(g2.params.size)
                  prefix.zip(g2.params).forall { case (p1, p2) =>
                    typeUniverse.hierarchy.isSubtype(p1.typ, p2.typ)
                  } &&
                    suffix.forall(p => typeUniverse.hierarchy.isSubtype(p.typ, v2Var.typ))
                }
                Acc(compatible, acc.hasAdvantage || compatible)

              case (Some(_), None) =>
                Acc(compatible = false, hasAdvantage = false)

              case (Some(v1Var), Some(v2Var)) =>
                val compatible = g1.params.size >= g2.params.size && {
                  val (prefix, suffix) = g1.params.splitAt(g2.params.size)
                  prefix.zip(g2.params).forall { case (p1, p2) =>
                    typeUniverse.hierarchy.isSubtype(p1.typ, p2.typ)
                  } &&
                    suffix.forall(p => typeUniverse.hierarchy.isSubtype(p.typ, v2Var.typ)) &&
                    typeUniverse.hierarchy.isSubtype(v1Var.typ, v2Var.typ)
                }
                val advantage = compatible && (
                  g1.params.size > g2.params.size ||
                    g1.params.zip(g2.params).exists { case (p1, p2) => p1.typ != p2.typ } ||
                    g1.params.drop(g2.params.size).exists(p => p.typ != v2Var.typ) ||
                    v1Var.typ != v2Var.typ
                  )
                Acc(compatible, acc.hasAdvantage || advantage)
            }
          }
      }

      result.compatible && result.hasAdvantage
    }
  }
}

final case class OverloadTable(variations: List[NativeFunctionDefinition]) {
  def findCandidates(typeUniverse: TypeUniverse,
                     query: SignatureQuery): List[NativeFunctionDefinition] = {
    variations.filter { variation =>
      variation.paramGroups.size == query.groups.size &&
        variation.paramGroups.zip(query.groups).forall { case (formalGroup, queryGroup) =>
          formalGroup.variadic match {
            case Some(v) =>
              queryGroup.parameters.size >= formalGroup.params.size && {
                val (fixedQuery, variadicQuery) = queryGroup.parameters.splitAt(formalGroup.params.size)
                formalGroup.params.zip(fixedQuery).forall {
                  case (formalParam, SignatureQueryParameter.OfType(queryType)) =>
                    typeUniverse.hierarchy.isSubtype(queryType, formalParam.typ)
                  case (_, SignatureQueryParameter.Unknown) =>
                    true
                } &&
                  variadicQuery.forall {
                    case SignatureQueryParameter.OfType(queryType) =>
                      typeUniverse.hierarchy.isSubtype(queryType, v.typ)
                    case SignatureQueryParameter.Unknown =>
                      true
                  }
              }
            case None =>
              formalGroup.params.size == queryGroup.parameters.size &&
                formalGroup.params.zip(queryGroup.parameters).forall {
                  case (formalParam, SignatureQueryParameter.OfType(queryType)) =>
                    typeUniverse.hierarchy.isSubtype(queryType, formalParam.typ)
                  case (_, SignatureQueryParameter.Unknown) =>
                    true
                }
          }
        }
    }
  }

  def resolveBestMatch(typeUniverse: TypeUniverse,
                       query: SignatureQuery): Either[ResolutionError, NativeFunctionDefinition] = {
    val candidates = findCandidates(typeUniverse, query)
    if (candidates.nonEmpty) {
      OverloadTable.resolveBestMatch(typeUniverse, candidates)
    } else {
      val conversionCandidates = findCandidatesWithConversions(typeUniverse, query)
      if (conversionCandidates.isEmpty) {
        Left(ResolutionError.NotFound)
      } else {
        val withCosts = conversionCandidates.map(c => (c, calculateTotalCost(typeUniverse, query, c)))
        val minCost = withCosts.map(_._2).min
        val bestByCost = withCosts.filter(_._2 == minCost).map(_._1)
        OverloadTable.resolveBestMatch(typeUniverse, bestByCost)
      }
    }
  }

  def resolveFunctionCall(typeUniverse: TypeUniverse,
                          argTypes: Vector[Vector[ProperType[TypeId]]]): Either[ResolutionError, NativeFunctionDefinition] =
    OverloadTable.resolveFunctionCall(typeUniverse, variations.toVector, argTypes)

  def resolveMethodCall(typeUniverse: TypeUniverse,
                        receiverType: ProperType[TypeId],
                        argTypes: Vector[Vector[ProperType[TypeId]]]): Either[ResolutionError, NativeFunctionDefinition] =
    OverloadTable.resolveMethodCall(typeUniverse, variations.toVector, receiverType, argTypes)

  def findCandidatesWithConversions(typeUniverse: TypeUniverse,
                                    query: SignatureQuery): List[NativeFunctionDefinition] = {
    variations.filter { variation =>
      variation.paramGroups.size == query.groups.size &&
        variation.paramGroups.zip(query.groups).forall { case (formalGroup, queryGroup) =>
          formalGroup.variadic match {
            case Some(v) =>
              queryGroup.parameters.size >= formalGroup.params.size && {
                val (fixedQuery, variadicQuery) = queryGroup.parameters.splitAt(formalGroup.params.size)
                formalGroup.params.zip(fixedQuery).forall {
                  case (formalParam, SignatureQueryParameter.OfType(queryType)) =>
                    typeUniverse.hierarchy.isSubtype(queryType, formalParam.typ) ||
                      TypeConversionGraph.conversionRelation(queryType, formalParam.typ) != TypeConversion.None
                  case (_, SignatureQueryParameter.Unknown) =>
                    true
                } &&
                  variadicQuery.forall {
                    case SignatureQueryParameter.OfType(queryType) =>
                      typeUniverse.hierarchy.isSubtype(queryType, v.typ) ||
                        TypeConversionGraph.conversionRelation(queryType, v.typ) != TypeConversion.None
                    case SignatureQueryParameter.Unknown =>
                      true
                  }
              }
            case None =>
              formalGroup.params.size == queryGroup.parameters.size &&
                formalGroup.params.zip(queryGroup.parameters).forall {
                  case (formalParam, SignatureQueryParameter.OfType(queryType)) =>
                    typeUniverse.hierarchy.isSubtype(queryType, formalParam.typ) ||
                      TypeConversionGraph.conversionRelation(queryType, formalParam.typ) != TypeConversion.None
                  case (_, SignatureQueryParameter.Unknown) =>
                    true
                }
          }
        }
    }
  }

  private def calculateTotalCost(typeUniverse: TypeUniverse,
                                 query: SignatureQuery,
                                 variation: NativeFunctionDefinition): Int = {
    variation.paramGroups.zip(query.groups).foldLeft(0) { case (acc, (formalGroup, queryGroup)) =>
      formalGroup.variadic match {
        case Some(v) =>
          val (fixedQuery, variadicQuery) = queryGroup.parameters.splitAt(formalGroup.params.size)
          val fixedCost = formalGroup.params.zip(fixedQuery).foldLeft(0) {
            case (acc2, (formalParam, SignatureQueryParameter.OfType(queryType))) =>
              val cost = if (typeUniverse.hierarchy.isSubtype(queryType, formalParam.typ)) {
                0
              } else {
                TypeConversionGraph.conversionRelation(queryType, formalParam.typ).cost
              }
              acc2 + cost
            case (acc2, (_, SignatureQueryParameter.Unknown)) =>
              acc2
          }
          val variadicCost = variadicQuery.foldLeft(0) {
            case (acc2, SignatureQueryParameter.OfType(queryType)) =>
              val cost = if (typeUniverse.hierarchy.isSubtype(queryType, v.typ)) {
                0
              } else {
                TypeConversionGraph.conversionRelation(queryType, v.typ).cost
              }
              acc2 + cost
            case (acc2, SignatureQueryParameter.Unknown) =>
              acc2
          }
          acc + fixedCost + variadicCost
        case None =>
          acc + formalGroup.params.zip(queryGroup.parameters).foldLeft(0) {
            case (acc2, (formalParam, SignatureQueryParameter.OfType(queryType))) =>
              val cost = if (typeUniverse.hierarchy.isSubtype(queryType, formalParam.typ)) {
                0
              } else {
                TypeConversionGraph.conversionRelation(queryType, formalParam.typ).cost
              }
              acc2 + cost
            case (acc2, (_, SignatureQueryParameter.Unknown)) =>
              acc2
          }
      }
    }
  }
}
